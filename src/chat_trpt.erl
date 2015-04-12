%%%-------------------------------------------------------------------
%%% @author William Yangzhu Yang <william.yang@ericsson.com>
%%% @copyright (C) 2015, William Yangzhu Yang
%%% @doc
%%%
%%% @end
%%% Created : 11 Apr 2015 by William Yangzhu Yang <william.yang@ericsson.com>
%%%-------------------------------------------------------------------
-module(chat_trpt).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([tcp_handler/1,
	 listen/1,
	 accept_loop/1]).

-define(SERVER, ?MODULE).

-record(state, {lport :: integer(),
	       listenerRef :: reference()}).
-define(ListenPort, 6667).
-define(ESCAPE,"#\\").
-define(INACTIVE_INIT_TIMEOUT, 60000).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Ref = start_listener(?ListenPort),
    {ok, #state{lport=?ListenPort, listenerRef = Ref}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', LRef, process, _Object, Info}, 
	    #state{listenerRef = LRef} = State) ->
    error_logger:error_report("tcp listener 'DOWN'! will restart"),
    NewRef = start_listener(State#state.lport),
    {noreply, State#state{listenerRef = NewRef}};
    
    
handle_info(Info, State) ->
    error_logger:info_report("Unknown INFO:", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_listener(Port) ->
    register(tcp_listener,spawn(?MODULE,listen,[Port])),
    erlang:monitor(process,tcp_listener).


-spec listen(integer()) -> ok | {error, Reason::term()}.
listen(Port) ->
    error_logger:info_msg("start listen on port: ~p ~n", [Port]),
    case gen_tcp:listen(Port,[binary,
			      {ip,{127,0,0,1}},
			      {active,2}
			     ]) of
	{ok, LSocket} ->
	    accept_loop(LSocket);
	{error,Reason} ->
	    throw({listen_error, Reason})
    end.


-spec accept_loop(inet:socket()) -> no_return().
accept_loop(LSocket) ->
    {ok,Socket} = gen_tcp:accept(LSocket),
    Pid = spawn(?MODULE,tcp_handler,[Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    erlang:monitor(process, Pid),
    accept_loop(LSocket).


tcp_handler(Socket) ->
    loop(Socket).

-spec loop(inet:socket()) -> no_return() | ok.
loop(Sock) ->
    receive
	{tcp, Socket, Data } ->
	    validate(Data) andalso
		recv_msg(Sock,strip_nl(Data)),
	    %%ok = gen_tcp:send(Sock,<< "welcome!",Data/binary>>),
	    loop(Socket);

	%% {tcp, SockOther, Data} ->
	%%     ok = gen_tcp:send(SockOther,<< "welcome!",Data/binary>>),
	%%     loop(Sock);

	{tcp_passive, Socket} ->
	    inet:setopts(Socket,[{active,10}]),
	    loop(Socket);
	
	{tcp_closed, Socket} ->
	    user_offline();

	Unknown ->
	    error_logger:error_report("Get tcp_passive: ~p~n", Unknown),
	    loop(Sock)

    after  ?INACTIVE_INIT_TIMEOUT ->
	    %%todo:disconnect
	    ok
    end.
	       

-spec recv_msg(Sock::inet:socket(),Bdata :: binary()) -> no_return().
recv_msg(Sock,<<?ESCAPE, Tail/binary>>) ->
    counter_step(cmd),
    cmd_actions(Sock,handle_cmd(Tail));

recv_msg(Sock,ChatMsg) ->
    case get(name) of
	 undefined ->
	    drop;
	_ -> %%found
	    boardcast(ChatMsg)
    end.

%%todo
cmd_actions(Sock,ActionList) ->
    [do_action(Act,Sock) || Act <- ActionList].


%%todo
do_action({replyto,Msg},Sock) ->
    gen_tcp:send(Sock,Msg).
    
    
%%todo
-spec handle_cmd(UserMsg :: binary()) -> [{Action :: atom() , Msg :: binary()}].
handle_cmd(<< "name:", NameRaw/binary>>) ->
    Name = strip_bs(NameRaw),
    case gen_server:call(chat_service,{user_reg,Name}) of
	ok ->
	    put(name,Name),
	    put(is_reg,true),
	    [{replyto,<< "Welcome ", Name/binary, "!\n" >>}];
	{error,Error} ->
	    BinErr = list_to_binary(Error),
	    [{replyto,<< "Sorry, ", BinErr/binary, "!\n" >>}]
    end;
    
handle_cmd(UnknownBin) ->
    io:format("unknown commad: ~p~n",[UnknownBin]),
    [].
	
%%todo
counter_step(_)->
    ok.

%%todo
boardcast(Msg) ->
    gen_server:cast(chat_service, {boardcast, Msg, get(name)}).

%%todo    
user_offline() ->
    Name = get(name),
    chat_service:user_offline(Name),
    ok. 

-spec strip_nl(binary()) -> binary().
strip_nl(Bin) ->
    Len = byte_size(Bin),
    binary:part(Bin,0,Len-1).

-spec validate(binary()) -> boolean().    
validate(<<$\n>>) ->  
    %%% blank space shall be dropped
    false;
validate(Bin) ->
    $\n == binary:last(Bin) .

-spec strip_bs(binary()) -> binary().
%%todo strip bs and tab
strip_bs(<<" ",Left/binary >>) ->
    strip_bs(Left);
strip_bs(Bin) -> 
    case binary:last(Bin) == " " of %%todo more effecy
    strip_bs(binary:part(Bin,0,byte_size(Bin)-1);

-ifdef(TEST).

strip_nl_test() ->
    ?assert(<< "hello world">>  == strip_nl(<<"hello world\n">>)).

validate_test_() ->
    [ ?_assertNot(validate(<<"\n">>)),
      ?_assertNot(validate(<<10>>)),
      ?_assertNot(validate(<<$\n>>)),
      ?_assert(validate(<<"hello world\n">>)),
      ?_assert(validate(<<"  hello world \n">>)),
      ?_assertNot(validate(<<"hello world\n ">>)),
      ?_assertNot(validate(<<"\n hello world \n ">>)),
      ?_assertNot(validate(<<"\jabn">>))
    ].

-endif.

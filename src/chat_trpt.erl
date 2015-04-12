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

-export([loop/2]). %%for process hibernate

-define(SERVER, ?MODULE).

-record(state, {lport :: integer(),
	       listenerRef :: reference()}).

-define(ListenPort, 6667).
-define(ESCAPE,"\$\\").
-define(INACTIVE_INIT_TIMEOUT, 15000).
-define(Regtimer, 10000).

-ifdef(TEST).
-undef(ListenPort).
-define(ListenPort, 6668).
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
handle_info({'DOWN', LRef, process, _Object, _Info}, 
	    #state{listenerRef = LRef} = State) ->
    error_logger:error_report("tcp listener 'DOWN'! restart now!"),
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
    {Pid,Ref} = spawn_monitor(?MODULE,listen,[Port]),
    register(tcp_listener,Pid),
    Ref.


-spec listen(integer()) -> ok | {error, Reason::term()}.
listen(Port) ->
    error_logger:info_msg("start listen on port: ~p ~n", [Port]),
    case gen_tcp:listen(Port,[binary,
			      {ip,{127,0,0,1}},
			      {active,10}]) 
    of

	{ok, LSocket} ->
	    accept_loop(LSocket);
	{error,eaddrinuse} ->
	    timer:sleep(1000),
	    listen(Port);
	{error,Reason} ->
	    throw({listen_error, Reason})
    end.


-spec accept_loop(inet:socket()) -> no_return().
accept_loop(LSocket) ->
    {ok,Socket} = gen_tcp:accept(LSocket),
    {Pid,_Ref} = spawn_monitor(?MODULE,tcp_handler,[Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    receive 
	{'DOWN',_Ref, process,_Pid,normal} ->
	    accept_loop(LSocket);
	{'DOWN',_Ref, process,_Pid,Reason} ->
	    counter_step({prodead,Reason}),
	    accept_loop(LSocket)
    after 0 ->
	    accept_loop(LSocket)
    end.



tcp_handler(Socket) ->
    {ok,Ref} = timer:send_after(?Regtimer, self(), regtimeout),
    loop(Socket,{Ref}).

-spec loop(inet:socket(), {timer:tref()}) -> no_return() | ok.
loop(Sock, {Ref} = State) ->
    receive
	{tcp_passive, Socket} ->
	    inet:setopts(Socket,[{active,10}]),
	    loop(Socket,State);

	{tcp, Socket, Data } ->
	    validate(Data) 
		andalso recv_msg(Sock,strip_nl(Data)),
		
	    loop(Socket,State);
	
	{tcp_closed, Sock} ->
	    user_offline();

	{send, Bin } ->
	    gen_tcp:send(Sock, Bin),
	    loop(Sock,State);

	regtimeout ->
	    gen_tcp:shutdown(Sock,read_write);
	
	stop_reg_timer ->
	    timer:cancel(Ref),
	    loop(Sock,{Ref});

	Unknown ->
	    io:format("Get Unknown: ~p~n", [Unknown]),
	    loop(Sock,State)

    after  ?INACTIVE_INIT_TIMEOUT ->
	    erlang:hibernate(?MODULE,loop,[Sock,State]),
	    ok
    end.
	       

-spec recv_msg(Sock::inet:socket(),Bdata :: binary()) -> no_return().
recv_msg(Sock,<<?ESCAPE, Tail/binary>>) ->
    counter_step(cmd),
    cmd_actions(Sock,handle_cmd(Tail));

recv_msg(_Sock,ChatMsg) ->
    case get(name) of
	 undefined ->
	    drop;
	_ -> %%found
	    boardcast(ChatMsg)
    end.

-type cmd_act() :: {sendto, Data:: binary()}.
-spec cmd_actions(Sock::inet:socket() , ActionList :: [cmd_act()]) -> no_return().
cmd_actions(Sock,ActionList) ->
    [do_action(Act,Sock) || Act <- ActionList].


do_action({replyto,Msg},Sock) ->
    gen_tcp:send(Sock,Msg).
    
-spec handle_cmd(UserMsg :: binary()) -> [{Action :: atom() , Msg :: binary()}].
handle_cmd(<< "name:", NameRaw/binary>>) ->
    Name = strip_bs(NameRaw),
    case gen_server:call(chat_service,{user_reg,Name}) of
	ok ->
	    put(name,Name),
	    put(is_reg,true),
	    stop_reg_timer(),
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

-spec boardcast(binary()) -> ok.
boardcast(Msg) ->
    chat_service:boardcast(Msg,get(name)).

-spec user_offline() -> ok.
user_offline() ->
    Name = get(name),
    chat_service:user_offline(Name),
    ok. 

-spec strip_nl(binary()) -> binary().
strip_nl(Bin) ->
    case binary:last(Bin) of
	$\n ->
	    Size = byte_size(Bin),
	    binary:part(Bin, 0,Size-1);
	_ ->
	    Bin
    end.
    
-spec validate(binary()) -> boolean().    
validate(<<$\n>>) ->  
    %%% blank space shall be dropped
    false;
validate(Bin) ->
    $\n == binary:last(Bin) .

-spec strip_bs(binary()) -> binary().
strip_bs(<<$\ ,Left/binary >>) ->
    strip_bs(Left);
strip_bs(<<$\t ,Left/binary >>) ->
    strip_bs(Left);
strip_bs(Bin) ->
    case binary:last(Bin) of
	Blank when Blank == $\  orelse Blank == $\t ->
	    Size = byte_size(Bin),
	    strip_bs(binary:part(Bin, 0,Size-1));
	_ ->
	    Bin
    end.


stop_reg_timer() ->	    
    self() ! stop_reg_timer.

-ifdef(TEST).

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

strip_nl_test_() ->
      [?_assert(strip_nl(<<"\njabn">>) ==  <<"\njabn">>),
       ?_assert(strip_nl(<<"jab\n">>) ==  <<"jab">>),
       ?_assert(strip_nl(<<"jab\n\n">>) ==  <<"jab\n">>),

       ?_assert(<< "hello world">>  == strip_nl(<<"hello world\n">>)),
       ?_assert(strip_nl(<<" jab \n \n">>) ==  <<" jab \n ">>)
       ].

strip_bs_test_() ->
      [?_assert(strip_bs(<<"jab">>) ==  <<"jab">>),
       ?_assert(strip_bs(<<" jab">>) ==  <<"jab">>),    
       ?_assert(strip_bs(<<" jab ">>) ==  <<"jab">>),    
       ?_assert(strip_bs(<<"jab ">>) ==  <<"jab">>),    
       ?_assert(strip_bs(<<"j a b ">>) ==  <<"j a b">>),    
       ?_assert(strip_bs(<<"   j a b   ">>) ==  <<"j a b">>),
       ?_assert(strip_bs(<<"   j a b\t   ">>) ==  <<"j a b">>),
       ?_assert(strip_bs(<<" \t  j a b\t   ">>) ==  <<"j a b">>)
       ].

-endif.

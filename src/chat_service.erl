%%%-------------------------------------------------------------------
%%% @author Zhuwei Yang <william.yang@ericsson.com>
%%% @copyright (C) 2015, Zhuwei Yang

%%% @doc
%%%
%%% @end
%%% Created : 11 Apr 2015 by Zhuwei Yang <william.yang@ericsson.com>
%%%-------------------------------------------------------------------
-module(chat_service).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% API for chat_trpt
-export([user_offline/1,
	 user_online/1,
	 boardcast/2]).

%% Internal export
-export([do_boardcast/2]).

%% Debug
-export([clean_table/1]).
-export([dbgapply/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(userinfo, {name :: binary(),
		   pid :: pid(),
		   chatrooms = [default] :: [binary() | default]
		  }).

	

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

-spec user_offline(Name :: binary()) -> ok|{error, any()}.
user_offline(Name) ->
    gen_server:call(?MODULE, {user_offline, Name}).

-spec user_online(Name :: binary()) -> ok|{error, any()}.
user_online(Name) ->
    gen_server:call(?MODULE, {user_reg, Name}).

-spec boardcast(Msg::binary(),FromUser::binary()) -> ok.
boardcast(Msg, FromUser) ->
    gen_server:cast(?MODULE, {boardcast, Msg, FromUser}).		       
		       

clean_table(Tab) ->
    gen_server:cast(?MODULE, {cleantab, Tab}).

dbgapply(M,F,A) ->
    gen_server:cast(?MODULE, {dbgapply,M, F,A}).

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
    error_logger:info_report("start chat_service ~n"),
    init_tabs(),
    {ok, #state{}}.

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
handle_call({user_reg,Name}, {Pid, _Ref} = _From, State) ->
    Reply = 
	case ets:lookup(regtab,Name) of
	    [] -> 
		U = #userinfo{name=Name, pid=Pid},
		ets:insert(regtab,U),
		join_chat_room(default,U),
		ok;
	    [{Name,Pid}] -> %%already registered
		ok;
	    [_] -> 
		{error,"username already exits, try other names!"}
	end,
		  
    {reply, Reply, State};


handle_call({user_offline,Name}, _From, State) ->
    Reply = ets:delete(regtab, Name),
    {reply, Reply, State};

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
%%todo what is name?
handle_cast({boardcast, Msg, User}, State) ->
    case flow_ctrl() of
	accept ->
	    spawn_monitor(?MODULE,do_boardcast,[User,Msg]);
	reject ->
	    drop
		
    end,
    {noreply, State};


handle_cast({cleantab,Name}, State) ->
    ets:delete_all_objects(Name),
    {noreply, State};

handle_cast({dbgapply,M,F,A}, State) ->
    apply(M,F,A),
    {noreply, State};

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
handle_info(_Info, State) ->
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
init_tabs() ->
    ets:info(regtab) == undefined 
	andalso ets:new(regtab,[set,
				protected,
				named_table,
				{keypos,2},
				{read_concurrency,true}
			       ]),
    case ets:info(chatrooms) of
	undefined ->
	    chatrooms= ets:new(chatrooms,[set,
					  protected,
					  {keypos,1},
					  {read_concurrency,true},
					  named_table
					 ]),
	    true = ets:insert(chatrooms,[{default,[]}]);
	_ ->
	    ok
    end,
		
    ok.
    

flow_ctrl() ->
    case flow_ctrl_pre() andalso msg_len_check() of
	true ->
	    accept;
	false ->
	    reject
    end.
	
-spec flow_ctrl_pre() -> boolean().
flow_ctrl_pre() ->
    case overload:request() of
	accept ->
	    counter_step(accpet),
	    true;
	reject ->
	    counter_step(reject),
	    false
    end.

msg_len_check() ->    
     case erlang:process_info(self(),message_queue_len) of
	 {message_queue_len, Len} when Len > 2000 ->  
	     %%we are overloaded
	     false;
	 _ ->
	     true
     end.
		 
do_boardcast(Username,Msg)->
    Rooms = get_chat_rooms(Username),
    send_to_rooms(Rooms, format_msg(Username,Msg)).

counter_step(_) ->
    todo.

join_chat_room(RoomName,UserInfo) ->
    case ets:lookup(chatrooms,RoomName) of
	[{RoomName, UserInfos}] ->
	    ets:insert(chatrooms,{RoomName,[ UserInfo | UserInfos]});
	[] ->
	    false
    end.

-spec get_chat_rooms(Name::binary()) -> [ChatRooms:: binary()].
get_chat_rooms(Name) ->
    case ets:lookup(regtab, Name) of
	[#userinfo{chatrooms=Rooms}]  ->
	    Rooms;
	_ ->
	    []
    end.


send_to_rooms(Rooms, Msg) ->
    [begin
	 [Pid ! {send,Msg} || Pid <- get_remote_pid_chat_room(R)]
      end || R <- Rooms].

-spec get_remote_pid_chat_room(RoomName::binary() |  default ) -> [ pid() ].
get_remote_pid_chat_room(RoomName) ->
    [{RoomName, Members}] = ets:lookup(chatrooms, RoomName),
    [Pid ||  #userinfo{pid=Pid} <- Members ].
    
    
-spec format_msg(binary(),binary()) -> binary().
format_msg(Sender,Msgbin) ->
    << "[",Sender/binary, ":]" , Msgbin/binary, "\n">>.
    

-ifdef(TEST).
format_msg_test_() ->
    [
     ?_assertEqual(format_msg(<<"william">>,<<"hi!">>), 
		   << "[william:]hi!\n" >>)
    ].

get_chat_rooms_test_() ->
    start_app(),
    [?_assertEqual(begin 	
		       ?MODULE:user_online(<< "will" >>),
		       get_chat_rooms(<< "will" >>) 	
		   end , [default] ),

     ?_assertEqual(begin 
		       ?MODULE:user_online(<< "hack" >>),
		       ?MODULE:user_offline(<< "hack" >>),
		       get_chat_rooms(<< "hack" >>)
		   end , [])
	      
    ].

get_remote_pid_chat_room_test() ->
    start_app(),
    ok = ?MODULE:user_online("Will"),
    ok = ?MODULE:user_online("Allice"),
    ok = ?MODULE:user_online("Bob"),
    get_remote_pid_chat_room(default) == [self(),self(),self()].
    
start_app() ->
    ok = application:ensure_started(sasl),
    ok = application:ensure_started(tcs).
    
-endif.

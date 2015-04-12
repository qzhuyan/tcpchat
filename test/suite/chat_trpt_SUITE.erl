%%%-------------------------------------------------------------------
%%% @author William Yangzhu Yang <william.yang@ericsson.com>
%%% @copyright (C) 2015, William Yangzhu Yang
%%% @doc
%%%
%%% @end
%%% Created : 12 Apr 2015 by William Yangzhu Yang <william.yang@ericsson.com>
%%%-------------------------------------------------------------------
-module(chat_trpt_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(NodeTcs, 'tcs@127.0.0.1').

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    %% connect to tcs app
    erlang:set_cookie(?NodeTcs, tcs),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               void() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               void() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [
     %%user login
     tc_user_online,
     tc_user_offline,
     tc_user_reg_timeout,
     %%user chat
     tc_chat_one_user,
     tc_chat_two_user_one_dir,
     tc_chat_two_user_two_dir,
     tc_chat_three_user_free_chat,

     tc_ratelimit
    ].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
tc_user_online() ->
    [{timetrap,{seconds,10}}].

tc_user_offline() ->
    [{timetrap,{seconds,10}}].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
tc_user_online(_Config) ->
    %%check if we get 'welcome message'
    {_S, {ok,"Welcome William!\n"}} = reg_user("William"),
    [_] = rpc:call(?NodeTcs, ets,lookup, [regtab,<<"William">>]).    

tc_user_offline(_Config) ->
    {S, {ok,"Welcome William!\n"}} = reg_user("William"),
    gen_tcp:close(S),
    [] == rpc:call(?NodeTcs, ets,lookup, [regtab,<<"William">>]).    

tc_user_reg_timeout(_Config) ->
    {ok,S} = gen_tcp:connect({127,0,0,1}, 6667, [{active,true}], 2000),
    receive 
	{tcp_closed, S} ->
	    ok
    after 12000 ->
	    ct:fail("didn't receive tcp close")
    end.
		
tc_chat_one_user(_Config) ->
    {SW, {ok,"Welcome William!\n"}} = reg_user("William"),
    gen_tcp:send(SW,"hello\n"),
    {ok,"[William:]hello\n"} = gen_tcp:recv(SW,0).


tc_chat_two_user_one_dir(_Config) ->
    {SW, {ok,"Welcome William!\n"}} = reg_user("William"),
    {SA, {ok,"Welcome Alice!\n"}} = reg_user("Alice"),
    gen_tcp:send(SW,"hello\n"),
    {ok,"[William:]hello\n"} = gen_tcp:recv(SW,0),
    {ok,"[William:]hello\n"} = gen_tcp:recv(SA,0).

tc_chat_two_user_two_dir(_Config) ->
    {SW, {ok,"Welcome William!\n"}} = reg_user("William"),
    {SA, {ok,"Welcome Alice!\n"}} = reg_user("Alice"),
    gen_tcp:send(SW,"hello\n"),
    {ok,"[William:]hello\n"} = gen_tcp:recv(SW,0),
    {ok,"[William:]hello\n"} = gen_tcp:recv(SA,0),
    gen_tcp:send(SA,"hello William!\n"),
    {ok,"[Alice:]hello William!\n"} = gen_tcp:recv(SW,0),
    {ok,"[Alice:]hello William!\n"} = gen_tcp:recv(SA,0).


tc_chat_three_user_free_chat(_Config) ->
    {SW, {ok,"Welcome William!\n"}} = reg_user("William"),
    {SA, {ok,"Welcome Alice!\n"}} = reg_user("Alice"),
    {SB, {ok,"Welcome Bob!\n"}} = reg_user("Bob"),
    gen_tcp:send(SW,"hello\n"),
    {ok,"[William:]hello\n"} = gen_tcp:recv(SW,0),
    {ok,"[William:]hello\n"} = gen_tcp:recv(SA,0),
    {ok,"[William:]hello\n"} = gen_tcp:recv(SB,0),
    gen_tcp:send(SA,"hello William!\n"),
    {ok,"[Alice:]hello William!\n"} = gen_tcp:recv(SW,0),
    {ok,"[Alice:]hello William!\n"} = gen_tcp:recv(SA,0),
    {ok,"[Alice:]hello William!\n"} = gen_tcp:recv(SB,0),
    gen_tcp:send(SB,"hello both!\n"),    
    {ok,"[Bob:]hello both!\n"} = gen_tcp:recv(SW,0),
    {ok,"[Bob:]hello both!\n"} = gen_tcp:recv(SA,0),
    {ok,"[Bob:]hello both!\n"} = gen_tcp:recv(SB,0).

tc_ratelimit(_Config) ->
    {SW, {ok,"Welcome Bob!\n"}} = reg_user("Bob"),
    {SA, {ok,"Welcome Alice!\n"}} = reg_user("Alice"),
    %%%  flooding the chat server with 100 pkts
    [gen_tcp:send(SW,integer_to_list(X)++"\n") || X<- lists:seq(1,100) ],
    {ok, Data} = gen_tcp:recv(SA,0),
    case length(Data) < length("[Bob:]1") *50 of
	true ->
	    ok;
	_ ->
	    ct:fail("Shall not recv so many data : ~p ~n", [length(Data)])
    end.
    
    
    

    
%%% help functions
reg_user(Name) ->
    {ok,S} = gen_tcp:connect({127,0,0,1}, 6667, [{active,false}], 2000),
    ok = gen_tcp:send(S,"#\\name:"++Name++"\n"),
    {S,gen_tcp:recv(S,0)}.



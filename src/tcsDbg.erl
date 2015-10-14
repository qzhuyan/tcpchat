-module(tcsDbg).

-export([start/0,
	 tpl/2,
	 tpl/3,
	 tpl/4,
	 prof/0,
	 trace_all_modules/0
	]).

-export([start_perfmon/1,
	 trace_msg_handler/2,
	 perfmon/1]).

-define(FUN, [{exception_trace},{caller}]).


start() ->
    case dbg:get_tracer() of
	{ok, Tracer} ->
	    ok;
	_ ->
	    dbg:tracer()
    end,
    dbg:p(all,[c]).
    

tpl(cr,M) ->
    start(),
    dbg:tpl(M,[{'_',[],?FUN}]).

tpl(cr,M,F) ->
    start(),
    dbg:tpl(M,F,[{'_',[],?FUN}]).

tpl(cr,M,F,A) ->
    start(),
    dbg:tpl(M,F,A,[{'_',[],?FUN}]).
    

trace_all_modules()->
    [tpl(cr, M) || M <- [chat_trpt,chat_service]].

prof() ->
    eprof:start_profiling(processes() -- [self()]).

start_perfmon(Interval) ->
    Pid = spawn_link(?MODULE,perfmon,[Interval]),
    register(perfmon,Pid),
    dbg:tracer(process,{fun trace_msg_handler/2,perfmon}).
    
    
trace_msg_handler({trace,Pid, call, {M,F,A}},Where) ->
    Where ! {M,F},
    Where;
trace_msg_handler(_,Where) ->
    io:format("unknown trace message"),
    Where.

perfmon(Interval) when is_integer(Interval)->
    timer:send_interval(Interval*1000,perfmon, print),
    perfmonloop([]).

perfmonloop(Counters)->
    receive 
	print ->
	    io:format("~p~n", [Counters]),
	    perfmonloop(Counters);
	reset ->
	    perfmonloop([]);
	{_,_} = CounterName ->
	    case lists:keyfind(CounterName,1,Counters) of
		false ->
		    perfmonloop([{CounterName,1} | Counters]);
		{_,Value} ->
		    NewCounters = 
			lists:keyreplace(CounterName, 1, Counters, {CounterName,Value+1}),
		    perfmonloop(NewCounters)
	    end
    end.
    




    




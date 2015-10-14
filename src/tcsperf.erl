-module(tcsperf).

-compile([export_all]).


start_perfmon(Interval) ->
    Pid = spawn_link(?MODULE,perfmon,[Interval]),
    register(perfmon,Pid),
    dbg:tracer(process,{fun trace_msg_handler/2,perfmon}).
    
trace_msg_handler({trace,Pid, call, {M,F,_A}},Where) ->
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
    


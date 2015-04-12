-module(tcsDbg).

-export([start/0,
	tpl/2,
	tpl/3,
	tpl/4,
	 trace_all_modules/0
	]).

-define(FUN, [{exception_trace},{caller}]).


start() ->
    dbg:tracer(),
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

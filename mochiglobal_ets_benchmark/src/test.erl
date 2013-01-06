%%%-------------------------------------------------------------------
%%% @author 纪秀峰 <jixiuf@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 2013-01-06 14:07 by 纪秀峰 <jixiuf@gmail.com>
%%  Last Updated: 纪秀峰 2013-01-06 14:17:40 星期日
%%%-------------------------------------------------------------------
-module(test).
-export([test/1,test_mochiglobal/1,test_ets/1]).

test(N)->
    test_ets(N),
    test_mochiglobal(N)
    .
test_ets(N)->
    ets:new(ets_tab,[named_table,set,{read_concurrency,true}]),
    ets:insert(ets_tab,{key,value}),
    {Time,ok}=timer:tc(fun()->test_ets_loop(N) end ),
    io:format("ets use time:~p~n",[Time]),
    ets:delete(ets_tab)
    .
test_ets_loop(0)->
    ok;
test_ets_loop(N)->
    _Value=ets:lookup(ets_tab,key),
    test_ets_loop(N-1)
    .


test_mochiglobal(N)->
    mochiglobal:put(key,value),
    {Time,ok}=timer:tc(fun()->test_mochiglobal_loop(N) end ),
    io:format("mochiglobal use time:~p~n",[Time])
    .
test_mochiglobal_loop(0)->
    ok;
test_mochiglobal_loop(N)->
    _Value=mochiglobal:get(key),
    test_mochiglobal_loop(N-1)
        .

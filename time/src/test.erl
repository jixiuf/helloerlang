%%%-------------------------------------------------------------------
%%% @author 纪秀峰 <jixiuf@gmail.com>
%%% @doc
%%% 取当前时间戳 now()较快一些
%%% @end
%%% Created : 2012-12-12 16:46 by 纪秀峰 <jixiuf@gmail.com>
%%  Last Updated: 纪秀峰 2012-12-12 16:54:23 星期三
%%%-------------------------------------------------------------------
-module(test).
-export([test/1]).
-export([get_seconds2/0,get_seconds1/0]).

test(N)->
    {T1,ok}=timer:tc(fun()-> [get_seconds1()||_T<- lists:seq(1,N)] ,ok end ),
    {T2,ok}=timer:tc(fun()-> [get_seconds2()||_T<- lists:seq(1,N)],ok end ),
    io:format("~p,~p~n",[T1,T2])
    .
get_seconds1()->
    {G,S,_M}=now(),
    G*1000000+S.

-define(SECONDS_1970,62167219200).
get_seconds2()->
    Dt=calendar:universal_time(),
    GSeconds=calendar:datetime_to_gregorian_seconds(Dt),
    GSeconds-?SECONDS_1970
    .

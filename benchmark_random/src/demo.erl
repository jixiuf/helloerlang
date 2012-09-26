%%%-------------------------------------------------------------------
%%% @author 纪秀峰 <jixiuf@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 2012-09-26 17:58 by 纪秀峰 <jixiuf@gmail.com>
%%%-------------------------------------------------------------------
-module(demo).
-export([start/0]).

start()->
    random:seed(now()),
    RandomT=timer:tc(fun()->
                     lists:foreach(fun(_E)->random:uniform(100)end,lists:seq(1,100000))
             end),
    io:format("100000 random:uniform used time:~p~n",[RandomT]),
    crypto:start(),
    RandomT2=timer:tc(fun()->
                     lists:foreach(fun(_E)->crypto:rand_uniform(0,100)end,lists:seq(1,100000))
             end),
    io:format("100000 crypto:rand_uniform used time:~p~n",[RandomT2])

        .

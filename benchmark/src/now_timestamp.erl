%%%-------------------------------------------------------------------
%%% @author 纪秀峰 <jixiuf@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 2013-01-25 10:21 by 纪秀峰 <jixiuf@gmail.com>
%%%-------------------------------------------------------------------
-module(now_timestamp).
-export([test/1]).

%% 测erlang:now() 与os:timestamp() 哪个速度更快
%% (emacs@jf.org)22> now_timestamp:test(10000).
%% erlang:now():{940,ok}
%% os:timestamp():{522,ok}
%% 测试结果显示 os:timestamp() 较快

test(N)->
    T=timer:tc(fun () -> now_benchmark(N) end),
    io:format("erlang:now():~p~n",[T]) ,
    T2=timer:tc(fun () -> os_timestamp_benchmark(N) end),
    io:format("os:timestamp():~p~n",[T2])
    .
now_benchmark(0)->ok;
now_benchmark(N)->
    now(),
    now_benchmark(N-1)
    .
os_timestamp_benchmark(0)->ok;
os_timestamp_benchmark(N)->
    os:timestamp(),
    os_timestamp_benchmark(N-1)
    .

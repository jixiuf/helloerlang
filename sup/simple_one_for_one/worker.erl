-module(worker).
-export([start_link/1]).

%% 似乎worker 的返回值必须是{ok,Pid}
start_link(S)->
    io:format("~p~n",[S]),
    {ok, spawn(fun()-> timer:sleep(1000) end)}
        .

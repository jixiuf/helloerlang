-module(log_execute).
-export([execute/1]).

execute(Sql)->
    io:format("~p~n",[Sql])
    .

-module(emysql_execute).
-export([execute/1]).

execute(Fun)->
    Fun()
    .

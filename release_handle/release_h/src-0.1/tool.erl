-module(tool).
-export([sayhello/0]).
-vsn("0.1").


sayhello()->
    io:format("this is just a common module withou process running on ,so ~n",[]),
    io:format("update this module is easy ~n",[])
        .

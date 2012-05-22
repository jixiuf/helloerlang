-module(tool).
-export([sayworld/0,sayhello/0]).
-vsn("0.2").


sayhello()->
    io:format("this is just a common module withou process running on ,so ~n",[]),
    io:format("update this module is easy ~n",[])
        .
sayworld()->
    io:format("this is just a common module withou process running on ,so ~n",[]),
    io:format("update this module is easy ~n",[])
    .

-module(mod_to_be_deleted).
-export([hello/0]).
-vsn("0.1").


hello()->
    io:format("this module will be deleted next version~n",[])
        .

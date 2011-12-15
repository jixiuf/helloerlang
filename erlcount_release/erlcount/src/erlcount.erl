-module(erlcount).
-export([start/2,stop/1]).

start(normal,_Args)->
    erlcount_log:info("erlcount is starting...~n",[]),
    erlcount_sup:start_link()
    .

stop(_State)->
    ok
    .

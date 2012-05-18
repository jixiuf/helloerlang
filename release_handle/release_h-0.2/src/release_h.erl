-module(release_h).
-export([start/2,stop/1]).
-vsn("0.1").


start(normal,_Args)->
    hello_sup:start_link()
        .

stop(_)->
    ok
        .

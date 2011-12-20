-module(main).
-export([start_link/0]).

start_link()->
    io:format("starting app...~n",[]),
    {ok,Supid}=   sup:start_link() ,
    supervisor:start_child(Supid,[" world"]),
    supervisor:start_child(Supid,[" world2"])
        .

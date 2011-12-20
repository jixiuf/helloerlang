-module(erlcount).
-export([start/2,stop/1,get_timestamp/0]).

get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    ((Mega*1000000+Sec)*1000000+Micro)/1000.

start(normal,_Args)->
    io:format("start time:~p~n",[get_timestamp()]),
    erlcount_log:info("erlcount is starting...~n",[]),
    erlcount_sup:start_link()
    .

stop(_State)->
    io:format("end time:~p~n",[get_timestamp()]),
    ok
        .

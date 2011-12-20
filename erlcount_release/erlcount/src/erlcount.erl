-module(erlcount).
-export([start/2,stop/1,get_timestamp/0]).

get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    ((Mega*1000000+Sec)*1000000+Micro)/1000.

start(normal,_Args)->
    CurrentTime=get_timestamp(),
    io:format("start time:~p~n",[CurrentTime]),
    erlcount_log:info("erlcount is starting...~n",[]),
    {ok,Pid}=erlcount_sup:start_link(),
    {ok,Pid,CurrentTime}
    .

stop(StartTime)->
    EndTime=get_timestamp(),
    io:format("end time:~p~n",[EndTime]),
    io:format("time used :~p~n",[EndTime-StartTime]),
    ok
        .

-module(distributed_application_app).

-export([start/2,stop/1]).
-define(app_name,?MODULE).

start(normal,_Args)->
    io:format("distributed_application_app start/2(normal) is running...~n",[]),
    Pid = spawn(fun()-> receive
                            _->
                                ok
                        end end),
    io:format("~p~n",[erlang:is_process_alive(Pid)]),
    global:register_name(distributed_application_app,Pid),
    {ok,Pid}
        ;
start({failover,Node},_Args)->
    io:format("failover: node:~p~n",[Node]),
    Pid = spawn(fun()-> receive
                            _->
                                ok
                        end end),
    {ok,Pid} ;
start({takeover,Node},_Args)->
    io:format("takeover: node:~p~n",[Node]),
    Pid = spawn(fun()-> receive
                            _->
                                ok
                        end end),
    {ok,Pid}

        .

stop(_State)->
    io:format("distributed_application_app is stopped~n",[]),
    ok
        .

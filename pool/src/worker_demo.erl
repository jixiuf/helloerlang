-module(worker_demo).
-compile([export_all]).
-behaviour(gen_server).

start_link(Delay,Msg)->
    gen_server:start_link(?MODULE,[Delay,Msg],[] )
        .

init([Delay,Msg])->
    io:format("a worker is initing...~n",[]),
    io:format("after ~p seconds ,this worker will die ,the message will be: ~p.~n",[Delay,Msg]),
    Pid = spawn(?MODULE,action,[Delay,Msg]),
        {ok,{Delay,Msg}}
        .


action(Delay,Msg)->
    timer:sleep(Delay*1000),
    io:format("ok,now time reach of ~p seconds with message ~p~n",[Delay,Msg])
        .

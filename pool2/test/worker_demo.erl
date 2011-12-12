-module(worker_demo).
-compile([export_all]).
-export([handle_info/2,init/1,terminate/2]).

start_link(Delay,Msg)->
    gen_server:start_link(?MODULE,[Delay,Msg],[] )
        .

init([Delay,Msg])->
    io:format("a worker is initing...~n",[]),
    io:format("after ~p seconds ,this worker will die ,the message will be: ~p.~n",[Delay,Msg]),
    Pid = spawn(?MODULE,action,[Delay,Msg]),
    erlang:monitor(process,Pid),
    {ok,{Delay,Msg}}
        .
handle_info({'DOWN',_Ref,process,_Pid,_Reason} ,_State)->
    %%当action/2 对应进程down ,会收到此信息，然后当前gen_server/退出，退出时ppool_serv 会收到信息，
    %%而后，从pool中清除此条process
    exit(normal)

        .

terminate(_M,_S)->
    ok
    .

action(Delay,Msg)->
    timer:sleep(Delay*1000),
    io:format("ok,now time reach of ~p seconds with message ~p~n",[Delay,Msg])

        .

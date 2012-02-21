-module(monitor).
-export([test1/0,test2/0]).

%% erlang:monitor(process,Pid),与 link 的不同是
%% link 是双向的，而monitor 是单向的。
%% link 收到的消息是{'EXIT', Exiting_Process_Id, Reason}
%% 而monitor 收到的消息是{'DOWN',Ref,process,Pid,Reason}

test1()->
    Pid=spawn(fun()->timer:sleep(1000) end ),
    io:format("the spawned pid is ~p~n",[Pid]),
    Ref=erlang:monitor(process,Pid),
    receive
        {'DOWN',Ref,process,Pid,Reason}->
            io:format("process down with reason :~p~n",[Reason])
                ;
        {'EXIT',Pid,Reason} ->
            io:format("process ~p exit with reason~p~n",[Pid,Reason])
    end
        .
%% 似乎对于spawn_link 与monitor 同时存在的时候，会收到两个信号
%% {'DOWN',Ref,process,Pid,Reason}
%% 与
%% {'EXIT',Pid,Reason}
%% 所以一般只用其中之一就可以了
test2()->
    Pid=spawn_link(fun()->timer:sleep(1000) end ),
    io:format("the spawned pid is ~p~n",[Pid]),
    Ref=erlang:monitor(process,Pid),
    receive
        {'DOWN',Ref,process,Pid,Reason}->
            io:format("process down with reason :~p~n",[Reason])
                ;
        {'EXIT',Pid,Reason} ->
            io:format("process ~p exit with reason:~p~n",[Pid,Reason])
    end,
    receive
        {'DOWN',Ref,process,Pid,Reason2}->
            io:format("process down with reason :~p~n",[Reason2])
                ;
        {'EXIT',Pid,Reason2} ->
            io:format("process ~p exit with reason:~p~n",[Pid,Reason2])
    end

        .
%% 　解除对进程的监视最好调用erlang:demonitor(Reference, [flush])，因为demonitor调用之前监视的进程可能就DOWN掉了。

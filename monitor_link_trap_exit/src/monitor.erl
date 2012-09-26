-module(monitor).
-export([test_exit_kill3/0,test_exit_kill2/0,test_exit_kill2/0,test_exit_kill/0,test1/0,test2/0]).
-export([test_exit_normal/0,test_exit_unnormal/0]).

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
%% 　解除对进程的监视最好调用erlang:demonitor(Reference, [flush])，因为
%% demonitor调用之前监视的进程可能就DOWN掉了。

%% 任一进程非正常退出，会给其link的
%% 进程集发出exit信号，exit信号将像多米诺骨牌一样传递出去（每张牌就是一个进程，
%% 倒掉代表进程被结束）。可以理解成：调用process_flag(trap_exit,true)后，进程将
%% 收到的其它进程exit信息转换成{'EXIT', Pid, Reason}消息，从而制止了多米诺骨牌
%% 的继续倒掉；
%% 　　　　进程的正常结束不会引发关注：进程正常退出时，也会给它的直
%% 接link set发送exit信号，但这个正常退出信号不会进一步的传播下去，也就是说正常
%% 退出信号不会引发多米诺骨牌的倒塌。（设置了trap_exit标志的进程会将这个exit信
%% 号转换成消息{'EXIT',Pid,normal}）。

%% 不论是normal or 非正常退出 ，link 与 trap_exit都是能够处理{'EXIT',Pid,Reason}
%% 信号的必要条件
test_exit_unnormal()->
    %% process_flag(trap_exit,true),
    Pid = Pid = spawn_link(fun()-> timer:sleep(1000) ,exit(shutdown)end),
    receive
        {'EXIT',Pid,Reason}->
            io:format("process :~p exit with reason:~p~n",[Pid,Reason])

    end
        .

%% 有人说:对于关联进程{'EXIT', Pid, Reason} Reason如果是kill,关联进程无论是否trap_exit都会死掉
%% 收到{'EXIT', Pid, Reason}消息
test_exit_kill()->
    process_flag(trap_exit,true),
    Pid = Pid = spawn_link(fun()-> timer:sleep(1000) ,exit(kill)end),
    receive
        {'EXIT',Pid,Reason}->
            io:format("process :~p exit with reason:~p~n",[Pid,Reason])

    end
        .

test_exit_kill2()->
    %% process_flag(trap_exit,true),
    %% 就算没有process_flag(trap_exit,true),
    %% 这里也能收到{'EXIT',Pid,Reason}消息
    Pid = Pid = spawn_link(fun()-> timer:sleep(1000) ,exit(kill)end),
    receive
        {'EXIT',Pid,Reason}->
            io:format("process :~p exit with reason:~p~n",[Pid,Reason])

    end
        .

test_exit_kill3()->
    %% process_flag(trap_exit,true),
    %% 这里不会收到任何消息，一直等待
    Pid = Pid = spawn(fun()-> timer:sleep(1000) ,exit(kill)end),
    receive
        {'EXIT',Pid,Reason}->
            io:format("process :~p exit with reason:~p~n",[Pid,Reason])

    end
        .


test_exit_normal()->
    process_flag(trap_exit,true),
    Pid = Pid = spawn_link(fun()-> timer:sleep(1000) ,exit(normal)end),
    receive
        {'EXIT',Pid,Reason}->
            io:format("process :~p exit with reason:~p~n",[Pid,Reason])

    end
        .

-module(monitor).
-export([test_exit_kill4/0,test_exit_kill3/0,test_exit_kill2/0,test_exit_kill/0,test1/0,test2/0]).
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

%% exit(Pid, Reason) -> true
%% Types:
%% Pid = pid()
%% Reason = term()
%% Sends an exit signal with exit reason Reason to the process Pid.
%% The following behavior apply if Reason is any term except normal or kill:
%% If Pid is not trapping exits, Pid itself will exit with exit reason Reason. If Pid is trapping exits, the exit signal is transformed into a message
%% {'EXIT', From, Reason} and delivered to the message queue of Pid. From is the pid of the process which sent the exit signal. See also process_flag/2.
%% If Reason is the atom normal, Pid will not exit. If it is trapping exits, the exit signal is transformed into a message {'EXIT', From, normal} and
%% delivered to its message queue.
%% If Reason is the atom kill, that is if exit(Pid, kill) is called, an untrappable exit signal is sent to Pid which will unconditionally exit with exit
%% reason killed.
%% 这句话是说exit(Pid,kill) 让Pid无条件退出，而Pid退出的Reason是会被设为killed ,如果有进程与它关联，将会收到{'EXIT', Pid, killed} (注意是killed,不是kill)
%% 这句话容易被误解
%% http://blog.programfan.info/tag/erlang/

test_exit_kill()->
    process_flag(trap_exit,true),
    %% 注意 exit(Reason) 与exit(Pid,Reason) 是有区别的
    %% erlang:exit
    Pid = Pid = spawn_link(fun()-> timer:sleep(1000) end),
    exit(Pid,kill),
    receive
        {'EXIT',Pid,Reason}->
            io:format("process :~p exit with reason:~p~n",[Pid,Reason])

    end.

%% test_exit_kill2与 test_exit_kill对比
%%证明对于link的进程erlang:exit/2 中Reason为kill
%% 都会收到{'EXIT',Pid,killed}不论有否trap_exit
%% test_exit_kill3辅证不进行link,则不会收到 消息

test_exit_kill2()->
    %% 就算没有process_flag(trap_exit,true),
    %% 似乎也收到了killed信号
    Pid = Pid = spawn_link(fun()-> timer:sleep(1000) end),
    exit(Pid,kill),
    receive
        {'EXIT',Pid,Reason}->
            io:format("process :~p exit with reason:~p~n",[Pid,Reason])

    end.

test_exit_kill3()->
    %% 就算没有process_flag(trap_exit,true),
    %% 不进行link就收不到了
    Pid = Pid = spawn(fun()-> timer:sleep(1000) end),
    exit(Pid,kill),
    receive
        {'EXIT',Pid,Reason}->
            io:format("process :~p exit with reason:~p~n",[Pid,Reason])

    end.

%% test_exit_kill4与test_exit_kill证明exit/2 与exit/1的区别
test_exit_kill4()->
    process_flag(trap_exit,true),
    Pid = Pid = spawn_link(fun()-> timer:sleep(1000) ,
                                   %% 注意这里是 进程自身退出，退出的原因是kill
                                   %% 文档中说的Reason为kill是指的erlang:exit/2中的reaon
                                   %% 而不是erlang:exit/1
                                   exit(kill)end),
    receive
        {'EXIT',Pid,Reason}->
            %% 你会看到这里收到的Reason是kill而不是killed
            io:format("process :~p exit with reason:~p~n",[Pid,Reason])

    end.



test_exit_normal()->
    process_flag(trap_exit,true),
    Pid = Pid = spawn_link(fun()-> timer:sleep(1000) ,exit(normal)end),
    receive
        {'EXIT',Pid,Reason}->
            io:format("process :~p exit with reason:~p~n",[Pid,Reason])

    end
        .

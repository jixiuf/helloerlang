-module(test_proc_lib).
-export([loop/1,init/0,start_link/0]).

%% http://www.cnblogs.com/me-sa/archive/2011/11/22/erlang0017.html
%% proc_lib 用于启动一个进程的函数包括
%% spawn系列，和start系列，前者异步，后者同步
%%使用start系列
%% 使用proc_lib会向启动的进程中添加一些内容:如，父进程 ,及此进程的mod funname ,argument_count

%% erlang:process_info(P2).
%% {dictionary,[{'$ancestors',[<0.45.0>]},
              %% {'$initial_call',{erl_eval,'-expr/5-fun-1-',0}}]},
%% 普通Erlang进程只有退出原因是normal的时候才会被认为是正常退出.
%% 由proc_lib启动的进程收到 shutdown ,{shutdown,Msg} 式的exit信息，认为进程是正常退出，不会记录崩溃报告

%% sys 模块包含了简单调试用行为实现的进程的函数。
%% 还有一些函数——要结合模块 proc_lib 中的函数——可以用于实现一种特殊进程，遵照OTP设计原则但不使用标准行为。它们也可以用于实现用户自定义的（非标准）行为。
%% sys 和 proc_lib 都属于 STDLIB应用。

%% sys 模块包含了简单调试用行为实现的进程的函数
start_link()->
    proc_lib:start_link(?MODULE,init,[])
    .

init()->
    io:format("init...~n",[]) ,
    %% 针对，proc_lib:start,proc_lib:start_link的同步调用（spawn,spawn_link不必，因其异步）
    %注意这里需要使用init_ack 向 Parent 进程报告"我已经启动完了，你可以返回了"
    %到此时，proc_lib:start,proc_lib:start_link才返回.
    proc_lib:init_ack({ok,self()}),
    loop(new_state)
    .

loop(State)->
    receive
        Msg->
            io:format("~p~n",[Msg]),
            loop(State)
    end
        .

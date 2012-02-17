% -*- coding:utf-8 -*-
-module(test2_client).
-export([call/1,start/0]).


%% gen_server ,handle_call中，使用{noreply,State}作为返回值,
%%而真正的业务逻辑交由worker_pid指向的进程，进行处理
%% 所以预期的效果是gen_server_pid 的消息队列不会激增，而
%% worker_pid的则会激增
%% 此例，仅仅减轻了gen_server的瓶颈，但实际的压力，则推向了worker_pid


start()->
    {ok,Pid}= test2_server:start_link(),
    register(gen_server_pid,Pid),
    %%每隔1ms 向gen_server_pid 发送一个请求
    timer:apply_interval(1, ?MODULE, call, [whereis(gen_server_pid)]),

    timer:sleep(1000),

    %% 每隔一秒钟打印gen_server_pid的消息队列长度
    timer:apply_interval(timer:seconds(1), util, pid_msg_info, [gen_server_pid]),
    %% 每隔一秒钟打印gen_server_pid的消息队列长度
    timer:apply_interval(timer:seconds(1), util, pid_msg_info, [worker_pid])
        .


call(Pid)->
    gen_server:call(Pid,now())
        .

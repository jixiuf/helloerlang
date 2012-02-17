% -*- coding:utf-8 -*-
-module(test2_client).
-export([call/1,start/0]).


start()->
    {ok,Pid}= test2_server:start_link(),
    register(gen_server_pid,Pid),
    %% 每隔一秒钟打印gen_server_pid的消息队列长度
    timer:apply_interval(timer:seconds(1), util, pid_msg_info, [gen_server_pid]),

    %%每隔1ms 向gen_server_pid 发送一个请求
    timer:apply_interval(1, ?MODULE, call, [whereis(gen_server_pid)])
        .


call(Pid)->
    gen_server:call(Pid,now())
    .

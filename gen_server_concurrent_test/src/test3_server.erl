%% -*- coding:utf-8 -*-
-module(test3_server).
-export([worker_loop/0,start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-define(MAX_WORKER,10).

start_link()->
    gen_server:start_link(?MODULE,[],[])
        .

init(S)->
    process_flag(trap_exit,true),


    %% pg2:create(worker_group),
    Workers=lists:map(fun(_Id)->
                              spawn(?MODULE,worker_loop,[])
                      end ,lists:seq(1,?MAX_WORKER)),
    put(workers,Workers),
    put(index,0),

    %% 每隔1s打印一次各进程状态(测试用代码)
    lists:foreach(fun(Pid)->
                          timer:apply_interval(timer:seconds(1), util, pid_msg_info, [Pid])
                  end
                  ,Workers),

    {ok, S}
        .

handle_call(Request,From,State)->
        %%从进程组中，随机获取一个进程
    Worker=get_random_worker(),
    Worker!{request,Request,From},
    {noreply, State}
        .

handle_cast(_Request,State)->
    {noreply, State} .

handle_info(_Info,State)->
    {noreply, State}.

terminate(_Reason,_State)->
    ok .

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .


worker_loop()->
    receive
        {request,Request,From}->
            timer:sleep(2000),                          %睡2s模拟费时操作。
            gen_server:reply(From,Request),
            %% io:format("gen_server got msg:~p~n",[Request])
            worker_loop() ;
        _ ->
            worker_loop()
    end

        .
get_random_worker()->
    Index= get(index),
    Workers= get(workers),
    put(index,(Index+1) rem ?MAX_WORKER),
    lists:nth(Index+1,Workers)
        .
%% 本来想用pg2模块来处理的
%% 可以借助于pg2模块，进程组的概念
%% http://blog.csdn.net/southflow/article/details/6868731
%% pg2:create(group1).

%% %%查看所有可见的组
%% pg2:which_groups().

%% %%同一个进程可以多次加入到组中，如果需要将一个进程加入到一个组中，如
%% pg2:join(group1,self()).
%% pg2:get_members(group1).
%% pg2:leave(group1,self()).

%% %%获取本节点内位于组内的进程
%% pg2:get_local_members(group1).

%% %%从进程组中，首先尝试获取位于进程组内、属于本地节点的pid ,如果没有才随机获
%% 取，但是测试时，所有节点都位于本地节点 ，并不随机 。所以最终放弃使用pg2
%% pg2:get_closest_pid(group1).

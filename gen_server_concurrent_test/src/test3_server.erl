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

    %% 每隔1s打印一次各进程状态
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
    put(index,(Index+1) rem 10),
    lists:nth(Index+1,Workers)
        .

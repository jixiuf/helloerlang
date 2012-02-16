% -*- coding:utf-8 -*-
-module(gens).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

start_link()->
    gen_server:start_link(?MODULE,[],[])
        .

init(S)->
    {ok, S}
        .

handle_call(hello,_From,State)->
    %% 对于{noreply,State}的handle_call,如果在handle_call中没有调用 gen_server:reply()进行回应的
    %% 话，则默认会5s 钟超时。
    io:format("you say hello,default :after 5s  ,it will exit with error.~n",[]),
    {noreply, State};
handle_call(world,From,State)->
    %%gen_server:reply第一个参数必须是handle_call回调函数中传过来的From

    %% gen_server:reply(From,you_say_world),
    %% {noreply, State};
    %% gen_server:reply+    {noreply, State}
    %% 等同于
    %% ==> {reply, Reply, State}
    %%但是使用{noreply}+gen_server:reply()的优势是
    %% 假如，noply会立即返回，但是如果此时gen_server:reply() 还没有执行的话
    %% gen_server:call() 还是会等待直到gen_server:reply() 执行完毕。

    %% gen_server在处理并发上不是很好，gen_server:call() 要排在一个队列里，
    %% 即前一个call 如果不返回的话，下一个call 是不会执行的。
    %% 所以使用使用gen_server:reply() + {noreply} 可以稍微改善一下
    %% 这种状况。{noreply ,}使当前进程立即返回，而 gen_server:reply()则在另一个进程里运行
    %% 执行耗时的运算后再返回,此时调用的call才会返回
    Pid = spawn(fun()->

                        io:format("this will waste a lot of time .,then call gen_server:reply() return Reply to client.~n",[]),
                        gen_server:reply(From,'you_say_world_with_gen_server:reply')
                end),
        {noreply, State};
handle_call(_Request,_From,State)->
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


%% {ok,P}=gens:start_link().
%% gen_server:call(P,hello).
%% gen_server:call(P,world).

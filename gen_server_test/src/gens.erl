% -*- coding:utf-8 -*-
-module(gens).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

%% 在gen_server主循环gen_server:loop中从消息队列中依次取出消息并处理，注意这是串行的，在
%% 一条消息处理完毕之前，不会处理下一条消息。

start_link()->
    {ok,Pid}=    gen_server:start_link(?MODULE,[],[]),
    io:format("pid=~p~n",[Pid]),
    {ok,Pid}
        .

init(S)->
    {ok, S}
        .

handle_call(hello,_From,State)->
    %% 对于{noreply,State}的handle_call,如果在handle_call中没有调用 gen_server:reply()进行回应的
    %% 话，则默认会5s 钟超时。
    io:format("you say hello,default :after 5s  ,it will exit for timeout.~n",[]),
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

    %% process_flag 与spawn_link 同时出现才起作用,注意当此种进程退出时，会发出{'EXIT',<0.59.0>,normal} 信号
    %% 在handle_info/2中可进行处理
    process_flag(trap_exit,true),
     spawn_link(fun()->

                   io:format("this will waste a lot of time .,then call gen_server:reply() return Reply to client.~n",[]),
                   timer:sleep(3000),
                   gen_server:reply(From,'you_say_world_with_gen_server:reply')
                end),
        {noreply, State};
handle_call(hi,From,State)->
    io:format("{reply,hi}~n",[]),
    io:format("self()=~p,From=~p~n",[self(),From]) ,
    {reply,"hi", State};
handle_call(shutdown,_From,State)->
    io:format("terminate/2 will be called~n",[]),
    {stop, "shutdown", "shutdown reply", State};
handle_call(_Request,_From,State)->             %似乎运行到这，对于{noreply} 的情况，会因为超时而使进程退出
    {noreply, State}
        .

handle_cast(hello,State)->
    io:format("say hello by cast~n",[]) ,
    {noreply, State} ;
handle_cast(_Request,State)->
    {noreply, State} .

%% messages that were sent directly with the ! operator and special ones like init/1's timeout,
%% monitors' notifications and 'EXIT' signals.
%% 对于直接使用! 发送过来的消息由handle_info()进行处理
%% 另外，init/1 的timeout
handle_info({'EXIT',_Pid,_Reason}=Info,State)->
    io:format("handle_info/2 is called & a process exited:~p~n",[Info]),
    {noreply, State};
handle_info(Info,State)->
    io:format("handle_info is called :~p~n",[Info]),
    {noreply, State}.

terminate(_Reason,_State)->
    %% 在这里进行一些清理工作
    io:format("terminalted~n",[]),
    ok .

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .


%% {ok,P}=gens:start_link().
%%A = gen_server:call(P,hello).
%%B = gen_server:call(P,world). % 同时会测试handle_info 处理EXIT
%%C = gen_server:call(P,hi).
%%D = gen_server:call(P,shutdown).
%% cast 为异步调用，不等等handle_cast返回，
%% gen_server:cast(P,hello).

%%test handle_info/2
%% 对于直接使用! 发送过来的消息由handle_info()进行处理

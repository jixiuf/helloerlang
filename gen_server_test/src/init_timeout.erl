% -*- coding:utf-8 -*-
-module(init_timeout).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([start_link/0]).
%% 测试init/1中返回值中Timeout的作用
start_link()->
    gen_server:start_link(?MODULE,[],[])
        .

init(S)->
    %% 3s
    Timeout=3000,
    {ok, S,Timeout}
        .

handle_call(_Request,_From,State)->
    {reply,hello, State}
        .

handle_cast(_Request,State)->
    {noreply, State} .

handle_info(timeout,State)->
    io:format("init timeout...~n",[]),
    {noreply, State}
    ;
handle_info(_Info,State)->
    io:format("handle_info/2 is called~n",[]) ,
    {noreply, State}.

terminate(_Reason,_State)->
    ok .

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .

%% {ok,P}=init_timeout:start_link().
%% init_timeout模块启动之后，必须在3s内，接到一个处理请求(只需要一个)，否则报超时错误

%% 而handle_call 与handle_cast ,handle_info中返回值包含Timeout的作用同理:
%% 表示处理完这次请求之后，在Timeout时间范围内必须接收到另一个请求(只需要一个)，
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State, Timeout}
%% 例子见handle_call_timeout.erl

%% gen_server:call(P,hello).

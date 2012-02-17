% -*- coding:utf-8 -*-
-module(handle_call_timeout).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
%% 而handle_call 与handle_cast ,handle_info中返回值包含Timeout的作用同理:
%% 表示处理完这次请求之后，在Timeout时间范围内必须接收到另一个请求(只需要一个)，否则报timeout错误，
%% 这个错误可以在handld_info/2中进行处理.
%%%        {reply, Reply, State, Timeout}
%%%        {noreply, State, Timeout}

%% gen_server:call(P,hello).
start_link()->
    gen_server:start_link(?MODULE,[],[])
        .

init(S)->
    {ok, S}
        .

handle_call(hello,_From,State)->
    io:format("after this request,you need another request in 3s ~n",[]) ,
    {reply,hello, State,3000}
        .

handle_cast(_Request,State)->
    {noreply, State} .

handle_info(_Info,State)->
    io:format("now timeout~n",[]) ,
    {noreply, State}.

terminate(_Reason,_State)->
    ok .

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .

%% {ok,P}=handle_call_timeout:start_link().
%% gen_server:call(P,hello).
%% 运行完这句后，3s内不要向其发送其他请求，则3s后会超时，然后handle_info会接到超时msg

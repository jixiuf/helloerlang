%% -*- coding:utf-8 -*-
-module(test2_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

start_link()->
    gen_server:start_link(?MODULE,[],[])
        .

init(S)->
    process_flag(trap_exit,true),
    {ok, S}
        .

handle_call(Request,From,State)->
    spawn(fun()->
                  timer:sleep(2000),                          %睡2s模拟费时操作。
                  gen_server:reply(From,Request)
                  %% io:format("gen_server got msg:~p~n",[Request])
          end ),
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

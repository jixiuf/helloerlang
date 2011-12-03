-module(ping).
-compile([export_all]).
-behaviour(gen_server).
%% http://learnyousomeerlang.com/clients-and-servers
start_link()->
    gen_server:start_link(?MODULE,0,[])         % this will return {ok,Pid} ,so you can  {ok,Pid} =ping:start_link().
        .
init(StateCount)->
    io:format("~p~n",[StateCount]),
    {ok,StateCount}
        .
ping(Pid)->
    Reply=  gen_server:call(Pid,ping),
    io:format("the reply is : ~p ~n",[Reply])
        .
stop(Pid)->
    Reply=  gen_server:call(Pid,stop),
    io:format("the reply is : ~p ~n",[Reply])
        .
%% 业务就而言，handle之类的方法 ，是服务器端,而ping() ,stop() 就留给客户端来调用的方法 。
handle_call(ping,From,StateCount)->
    io:format("~p:~p~n",[pong,StateCount+1]),
    {reply,pong,1+StateCount};                  %{reply,Reply,State}
handle_call(stop,From,StateCount) ->
    {stop,no_reason_just_stop,just_stop,StateCount} %{stop,Reason,Reply,State} ,之后会此用下面的 terminate()
        .

handle_cast(Msg,StateCount)->
    io:format("~n",[]).

handle_info(P1,P2)->                            % random msg
    io:format("~n",[])
        .
terminate(Reason,StateCount)->
    io:format("look it it terminated ,reason: ~p~n",[Reason])
        .

code_change(Previous_Version,StateCount,Extra)->
    io:format("~n",[])
        .
%% test 1
%% {ok,Pid} =ping:start_link().
%% ping:ping(Pid).
%% ping:stop(Pid).

-module(ping).
-compile([export_all]).
-behaviour(gen_server).
%% http://learnyousomeerlang.com/clients-and-servers
start_link()->
    gen_server:start_link(?MODULE,0,[])         % this will return {ok,Pid} ,so you can  {ok,Pid} =ping:start_link().
        .
init(StateCount)->
    io:format("~p~n",[StateCount]),

    process_flag(trap_exit,true),
    spawn_link(?MODULE,test,[]),                %此两句用来测试 handle_info 用来处理EXIT信号

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

%% messages that were sent directly with the ! operator and special ones like init/1's timeout,
%% monitors' notifications and 'EXIT' signals.

handle_info({'EXIT',Pid,Reason},State) ->
    io:format("exittttttt~n",[]),
    {noreply,State}
        ;
handle_info(Msg,State)->                            % random msg ->
    io:format("random msg I don't want to handle,msg:~p~n",[Msg]),

    {noreply,State}
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
%% Pid! random msg
test()->
    timer:sleep(5000),
    exit(normal)
        .

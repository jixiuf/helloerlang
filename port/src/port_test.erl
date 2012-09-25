%%%-------------------------------------------------------------------
%%% @author 纪秀峰 <jixiuf@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 2012-09-25 18:56 by 纪秀峰 <jixiuf@gmail.com>
%%%-------------------------------------------------------------------
-module(port_test).

-export([test/0]).
-export([sync_cmd/1]).
-define(EXIT_STATUS_SUCCESS,0).

%% os:cmd主要用于一次行的交互，open_port可以通过标准输入和输出做各种各样的长时间的交互。
%% http://blog.yufeng.info/archives/1703
%% 同步执行，等待命令执行完毕
-spec sync_cmd(list())->iodata()|error.
sync_cmd(Cmd)->
    Monitor=self(),
    spawn(fun () ->
                  Port=open_port({spawn ,Cmd}, [stream,exit_status]),
                  Monitor!{recv ,recv_data(Port,[]) }
          end),
    receive
        {recv,{ok,Data}}->
            Data;
        {recv,error}->
            error;
        _ ->
            error
    end
        .
recv_data(Port,SoFar)->
    receive
        {Port,{data,Data}}->
            recv_data(Port,[Data|SoFar]);
        {Port,closed} ->
            %% Reply to Port ! {Pid,close}.
            {ok,lists:reverse(SoFar)};
        {Port,connected} ->
            %%	Reply to Port ! {Pid,{connect,NewPid}}
            %% 不会走到此处,将一个Port与另外一个pid关联时才会收到此消息
            recv_data(Port,SoFar);
        {Port, {exit_status, ?EXIT_STATUS_SUCCESS}}->
            {ok,lists:reverse(SoFar)};
        {Port, {exit_status, Status}}->
            io:format("exit status:~p~n",[Status]) ,
            {ok,lists:reverse(SoFar)};
        {'EXIT',Port,Reason} ->
            %% the port has terminated for some reason
            io:format("reason:~p~n",[Reason]) ,
            {error_exit,lists:reverse(SoFar)};
        RandomMsg->
            io:format("~p~n",[RandomMsg]) ,
            recv_data(Port,SoFar)
    end.

test()->
    IOData=sync_cmd("ls /tmp|grep er"),
    io:format("~s~n",[iolist_to_binary(IOData)])
    .

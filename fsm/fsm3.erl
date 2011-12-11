-module(fsm3).
-behaviour(gen_fsm).
-compile([export_all]).
%% 有限状态机
%% 对于StateName/2 的方法 ，由gen_fsm:send_event/2来触发,此为异步发送
%% 对于StateName/3 的方法 ，由gen_fsm:sync_send_event/3来触发 此为同步发送
%%似乎要用 {reply,my_reply,state1,"state data of state1."}作返回值
%% 我用{next_state,state1,"state data of state1."}作返回值，报timeout
%% {next_state, NextStateName, NewStateData} 当返回next_state 的状态时 必须用    gen_fsm:reply(From,my_reply), 先给一个回应

%%statename/3 可以返回
%% {reply, Reply, NextStateName, NewStateData}
%% {reply, Reply, NextStateName, NewStateData, Timeout}
%% {reply, Reply, NextStateName, NewStateData, hibernate}
%% {next_state, NextStateName, NewStateData} 当返回next_state 的状态时 必须用    gen_fsm:reply(From,my_reply), 先给一个回应
%% {next_state, NextStateName, NewStateData, Timeout}
%% {next_state, NextStateName, NewStateData, hibernate}
%% {stop, Reason, Reply, NewStateData}
%% {stop, Reason, NewStateData}

%%
start()->
    {ok,Pid}=    gen_fsm:start_link(?MODULE,init_data,[]),
    register(server,Pid)
        .

init(State)->
    io:format("init... now StateData is ~p.~n",[State]),
    {ok,state1,State}.                         %初始state 为init_state ,下一state will be state1,即state1/2 会被调用.

state1(event1,StateData) ->
    timer:sleep(1000),
    io:format("~ts~n",["server receive msg after 3 second ,server print this line"]),
    {next_state,state1,"state data of state1."}
        .

state1(event1,From,StateData) ->
    timer:sleep(1000),
    io:format("~ts~n",["server receive msg after 3 second ,server print this line"]),
    %% gen_fsm:reply(From,my_reply),
    %% {next_state,state1,"state data of state1."}
    %%或者
    {reply,my_reply,state1,"state data of state1."}
        .

send1()->
    gen_fsm:send_event(whereis(server),event1),
    io:format("after send event , client print this line .~n",[]).

send2()->
    Reply=gen_fsm:sync_send_event(whereis(server),event1),
    io:format("after send event , client print this line ~p .~n",[Reply]).

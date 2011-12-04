-module(fsm3).
-behaviour(gen_fsm).
-compile([export_all]).
%% 有限状态机
%% 对于StateName/2 的方法 ，由gen_fsm:send_event/2来触发,此为异步发送
%% 对于StateName/3 的方法 ，由gen_fsm:sync_send_event/3来触发 此为同步发送
%%似乎要用 {reply,my_reply,state1,"state data of state1."}作返回值
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
    {reply,my_reply,state1,"state data of state1."}
        .

send1()->
    gen_fsm:send_event(whereis(server),event1),
    io:format("after send event , client print this line .~n",[]).

send2()->
    Reply=gen_fsm:sync_send_event(whereis(server),event1),
    io:format("after send event , client print this line ~p .~n",[Reply]).

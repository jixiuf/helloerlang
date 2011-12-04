-module(fsm2).
-behaviour(gen_fsm).
-compile([export_all]).
%% 有限状态机
%% 处理handle_event .global event
%%
start()->
    gen_fsm:start_link(?MODULE,init_data,[])
        .

init(State)->
    io:format("init... now StateData is ~p.~n",[State]),
    {ok,state1,State}.                         %初始state 为init_state ,下一state will be state1,即state1/2 会被调用.

state1(event1,StateData) ->
    io:format("now is state1.next state will be state2 .StateData: ~p,Event msg is :~p",[StateData,event1]),
    {next_state,state2,"state data of state1."}
        .

state2(event2,StateData)->                          %匹配从state1 转过的的。
    io:format("now is state2,next state will be state1.. StateData: ~p,Event msg is :~p",[StateData,event2]),
    {next_state,state1,"state data of state2."}
        .
%%正常的情况，只有处于某一特定state 收到某一特定事件才会触发相应操作
%处理global 事件 ，即不论当前状态为何，只要收到某一特定global事件， 就会触发此方法。此类事件由
handle_event(Event, StateName, StateData)->
    io:format("handle global event . event:~p,statename:~p,stateData:~p~n",[Event,stateData,StateData]),
    {next_state, state1, "back to state1"}        .

%% {ok,P}=fsm2:start()
%% gen_fsm:send_event(P,event1). % after init , send this
%% gen_fsm:send_event(P,event2). % then , send this
%%gen_fsm:send_all_state_event(P,a_global_event)

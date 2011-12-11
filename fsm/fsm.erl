-module(fsm).
-behaviour(gen_fsm).
-compile([export_all]).
%% 有限状态机

start()->
    gen_fsm:start_link(?MODULE,[],[])
        .

init(_State)->
    io:format("init...~n",[]),
    {ok,state1,"init data"}.                         %初始state 为init_state ,下一state will be state1,即state1/2 会被调用.

state1(Event,StateData) ->
    io:format("now is state1.next state will be state2 .StateData: ~p,Event msg is :~p",[StateData,Event]),
    {next_state,state2,"state data of state1."}
        .

state2(Event,StateData)->                          %匹配从state1 转过的的。
    io:format("now is state2,next state will be state1.. StateData: ~p,Event msg is :~p",[StateData,Event]),
    {next_state,state1,"state data of state2."}
        .

%% gen_fsm:send_event(P,"event msg").

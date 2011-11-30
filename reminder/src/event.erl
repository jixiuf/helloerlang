-module(event).
-compile([export_all]).
-record(state,{server,name="",to_go=0}).

init(Server,EventName,DateTime)->
    loop(#state{server=Server,name=EventName,to_go=time2seconds_normalize(DateTime)})
        .
start_link(EventName,To_go_SecondsDateTime)->
        debug:debug("event"," a event named: " ++ EventName++ " is running..."),
    spawn_link(?MODULE,loop,[#state{server=self(),name=EventName,to_go=time2seconds_normalize(To_go_SecondsDateTime)}])
        .
start(EventName,To_go_SecondsDateTime)->
        debug:debug("event"," a event named: " ++ EventName++ " is running..."),
    spawn(?MODULE,loop,[#state{server=self(),name=EventName,to_go=time2seconds_normalize(To_go_SecondsDateTime)}])
        .
cancel(Pid)->
    debug:debug("event"," event is canceling ..." ),
    Ref=erlang:monitor(process,Pid),            %对进程进行监视，如果监视的进程死亡会收到{'DOWN',Ref,process,Pid,Reason}
    Pid!{self(),Ref,cancel},
    receive
        {Ref,ok}->
            %% erlang:demonitor(Ref,[flush]);
            erlang:demonitor(Ref,[]),
            debug:debug("event","ok,the event is canceld now!~n"),
            ok;
        {'DOWN',Ref,process,Pid,_} ->
            debug:debug("event","ok,event_already_done!~n"),
            ok
    end.



loop(State=#state{server=Server,to_go=[T1|Rest]})->
    receive
        {Server,Ref,cancel}->
        debug:debug("event","got a cancel sign ."),
            Server! {Ref,ok}
    after T1*1000 ->
            if Rest =:=[] ->
                    debug:debug("event","event is done."),
                    Server ! {done,State#state.name};
               Rest =/= [] ->
                    debug:debug("event","time is passing by."),
                    loop(State#state{to_go=Rest})
            end
    end.
    %% loop1({State,normalize(State#state.to_go)}).

normalize(N)->
    %% Limit = 49*24*60*60,                         %49，天。一个timer 的超时时间最长只能是50天，为了使超时可接受，50天以上的，分段进行，
    Limit=2,
    case  N rem Limit of
        0 ->  lists:duplicate((N div Limit) ,Limit); %整除的情况
        _ -> [(N rem Limit)| lists:duplicate((N div Limit) ,Limit)]
    end.
%% lists:duplicate(3,a)=[a,a,a] 知识点

%% event:time2seconds_normalize({{2012,11,28},{22,03,01}}).
%% time2seconds_normalize and normalized it .
time2seconds_normalize(OutTime={{_,_,_},{_,_,_}})->
    Now=calendar:local_time(),
    Seconds = calendar:datetime_to_gregorian_seconds(OutTime)-
        calendar:datetime_to_gregorian_seconds(Now),
    Sec= if Seconds>0 -> Seconds;
            Seconds =< 0 ->0 end,
    normalize(Sec)
        .

test_error()->
    process_flag(trap_exit,true),
    spawn_link(event,loop,[#state{server=self(),name="hello",to_go=a}]),
    receive
        {'EXIT',From,Reason}->
            io:format("got exit msg:~p:~p~n",[From,Reason]);
        %% error:Error->
        %%     io:format("超时时间格式不对，不是一个数字，请check,~p~n",[Error]);
        Msg ->
            io:format("other msg:~p~n",[Msg])
    after 10000->
            timeout
    end.

%% rr(event).
%% P3=spawn(event,loop,[#state{server=self(),name="hel",to_go=40}]).
%% R3 = make_ref().
%% P3 !{self(),R3,cancel}
%% flush().

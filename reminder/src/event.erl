-module(event).
-compile([export_all]).
-record(state,{server,name="",to_go=0}).

start(EventName,To_go_Seconds)->
    spawn_link(?MODULE,loop,[#state{server=self(),name=EventName,to_go=normalize(To_go_Seconds)}]),
    receive
        Msg ->
            io:format("~p~n",[Msg])
    end
        .

loop(State=#state{server=Server,to_go=[T1|Rest]})->
    receive
        {Server,Ref,cancel}->
            Server! {Ref,ok}
    after T1*1000 ->
            if Rest =:=[] ->
                    Server ! {done,State#state.name};
               Rest =/= [] ->
                    io:format("debug:a tmp timer done.~n",[]),
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

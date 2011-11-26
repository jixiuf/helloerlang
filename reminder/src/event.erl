-module(event).
-compile([export_all]).
-record(state,{server,name="",to_go=0}).

loop(State)->
    loop1({State,normalize(State#state.to_go)}).
    %% case catch of
    %%     {'EXIT',W}->
    %%         io:format("got exit msg:~p~n",[W]),
    %%         exit(you_must__catch_this_exit_msg);
    %%     %% error:Error->
    %%     %%     io:format("超时时间格式不对，不是一个数字，请check,~p~n",[Error]);
    %%     Msg ->
    %%         io:format("other msg:~p~n",[Msg]),
    %%         exit(exit)
    %% end.

normalize(N)->
    %% Limit = 49*24*60*60,                         %49，天。一个timer 的超时时间最长只能是50天，为了使超时可接受，50天以上的，分段进行，
    Limit=2,
    case  N rem Limit of
        0 ->  lists:duplicate((N div Limit) ,Limit); %整除的情况
        _ -> [(N rem Limit)| lists:duplicate((N div Limit) ,Limit)]
    end.
%% lists:duplicate(3,a)=[a,a,a] 知识点

loop1 ({State=#state{server=Server},[T1]}) ->
    receive
        {Server,Ref,cancel}->
            Server! {Ref,ok}
    after T1*1000 ->
            Server ! {done,State#state.name}
    end;
loop1 ({State=#state{server=Server},[T1|Rest]}) ->
    receive
        {Server,Ref,cancel}->
            Server! {Ref,ok}
    after T1*1000 ->
            %% io:format("debug:a tmp timer done.~n",[]),
            loop1({State,Rest})
    end.

test()->
    process_flag(trap_exit,true),
    spawn_link(event,loop,[#state{server=self(),name="hello",to_go=a}]),
    receive
        {'EXIT',F,Reason}->
            io:format("got exit msg:~p~n",[Reason]);
        %% error:Error->
        %%     io:format("超时时间格式不对，不是一个数字，请check,~p~n",[Error]);
        Msg ->
            io:format("other msg:~p~n",[Msg])
    after 10000->
            timeout
    end.

-module(eventserver).
-compile([export_all]).
-record(state,{clients,events}).
-record(event,{name="",pid,desc="",timeout={{1970,01,01},{0,0,0}}}).

listen(DelaySeconds)->                          %接收 DelaySeconds内收到的所有提醒
    receive
        M={done,_EventName,_Desc}->
            [M|listen(0)]
    after DelaySeconds*1000->
              []
    end
        .
subscribe(Pid)->                                %此由client 进行调用
    Ref = erlang:monitor(process,whereis(?MODULE)),
    ?MODULE ! {Pid,Ref,{subscribe,self()}},     %此由client 进行调用,故self ()表示client Pid
    receive
        {Ref,ok}->
            {ok,Ref};
        {'DOWN',Ref,process,_Pid,Reason} ->
            {error,Reason}
    after 5000->
            {error,timeout}
    end.

addevent(EventName,Desc,TimeoutDateTime)->      %此由client 进行调用,
    MsgRef = make_ref(),
    debug:debug("client","add a event named" ++ EventName),
    ?MODULE !{self(),MsgRef,{addevent,EventName,Desc,TimeoutDateTime}},%此由client 进行调用,故self ()表示client Pid
    receive
        {MsgRef,ok}->
            {ok,MsgRef};
        {MsgRef,{error,bad_timeout}} ->
            {error,bad_timeout}
    after 5000->
            {error,timeout}
    end.
cancelEvent(EventName)->
    Ref = make_ref(),
    ?MODULE !{self(),Ref,{cancelEvent,EventName}},
    receive
        {error,event_doesnt_exist}->
            {error,event_doesnt_exist};
        {Ref,ok} ->
            ok
    after 5000->
            {error,timeout}

    end
        .
shutdown()->%此由client 进行调用,
    ?MODULE !shutdown.

codechange()->%此由client 进行调用,
    ?MODULE !codechange.

%% 以上方法的存在是因为，"不要将消息内容暴露给外部,而只是提供相应的接口供其调用".
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start()->
    register (?MODULE, Pid= spawn(?MODULE,init ,[])),
    Pid .

start_link()->
    register (?MODULE, Pid= spawn_link(?MODULE,init ,[])),
    Pid .

%% orddict 文档 http://learnyousomeerlang.com/a-short-visit-to-common-data-structures#key-value-stores
%% orddict是一种key-value 的处理方式，适于数量不是很大的情况,大约在75左右时最佳
init()->
    loop(_State=#state{clients=orddict:new(),events=orddict:new()})
        .

loop(State=#state{})->
    receive
        {Pid,MsgRef,{subscribe,ClientPid}}->    %定阅
            ClientRef=erlang:monitor(process,ClientPid),   %对client进程监控，以便它死时，移除之
            NewClients=orddict:store(ClientRef,ClientPid,State#state.clients) ,% orddict:store(key,val,dict) ,put
            Pid!{MsgRef,ok},
            loop(State#state{clients=NewClients}) ;
        {Pid,MsgRef,{addevent,EventName,Desc,TimeoutDateTime}}-> %add a new event
            case valid_datetime(TimeoutDateTime) of
                true->
                    debug:debug("server"," a event named" ++ EventName++ "is added"),
                    EventPid=event:start_link(EventName,TimeoutDateTime),
                    NewEvents=orddict:store(EventName,#event{name=EventName,pid=EventPid,desc=Desc,timeout=TimeoutDateTime},State#state.events),
                    Pid!{MsgRef,ok},
                    loop(State#state{events=NewEvents});
                false ->
                    Pid! {MsgRef,{error,bad_timeout}},
                    loop(State)
            end ;
        {Pid,MsgRef,{cancelEvent,EventName}}->  % cancel an event
            case orddict:find(EventName,State#state.events) of
                error->
                    Pid! {MsgRef,{error,event_doesnt_exist}},
                    loop(State);
                {ok,Event}->
                    event:cancel(Event#event.pid),
                    NewEvents=orddict:erase(EventName,State#state.events),
                    Pid!{MsgRef,ok},
                    loop(State#state{events=NewEvents})
            end;
        {done,EventName}->                      % event done
            Event=orddict:fetch(EventName,State#state.events),
            send2clients({done,Event#event.name,Event#event.desc},State#state.clients),
            loop(State#state{events=orddict:erase(EventName,State#state.events)}),
            %% Done: send info to clients
            io:format("event:~p is done!",[EventName]);
        shutdown ->
            exit(shutdown);
        {'DOWN',ClientRef,process,_Pid,_Reason} -> % when client die
            loop(State#state{clients=orddict:erase(ClientRef,State#state.clients)});
        codechange ->
            ?MODULE:loop(State);                %module内部，通过 ?MODULE调用内部的方法时，会加载新版的此模块，此处不能写成 loop(State);
        _Unknow ->
            io:format("UnKnow msg",[]),
            loop(State)
    end
        .
send2clients(Msg,Clients)->
    orddict:map(fun (_ClientRef,ClientPid) -> ClientPid ! Msg end ,Clients )
        .


%% 对日期进行检醒 calendar 只提供了calendar:valid_date/1,对日期进行检查 ，未提供对HH:mm:ss部分进行
valid_datetime({Date,Time})->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause->                     % not in {{Y,M,D},{H,m,s}} format
            false
    end;
valid_datetime(_) ->
    false.
valid_time({H,M,S})->
    valid_time(H,M,S).
valid_time(H,M,S) when H>=0 ,H<60,
                       M>=0,M<60,
                       S>=0,S<60
                       ->true;

valid_time(_,_,_) ->false.

-module(eventserver).
-compile([export_all]).
-record(state,{clients,events}).
-record(event,{name="",pid,desc="",timeout={{1970,01,01},{0,0,0}}}).

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
                    EventPid=event:start_link(EventName,TimeoutDateTime),
                    NewEvents=orddict:store(EventName,#event{name=EventName,pid=EventPid,desc=Desc,timeout=TimeoutDateTime},State#state.events),
                    Pid!{MsgRef,ok},
                    loop(State#state{events=NewEvents});
                false ->
                    Pid! {MsgRef,{error,bag_timeout}},
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
            %% DONE: send info to clients
            io:format("event:~p is done!",[EventName]);
        shutdown ->
            exit(shutdown);
        {'DOWN',ClientRef,process,_Pid,_Reason} -> % when client die
            loop(State#state{clients=orddict:erase(ClientRef,State#state.clients)});
        codechange ->
            loop(State);
        Unknow ->
            io:format("UnKnow msg",[]),
            loop(State)
    end
        .
send2clients(Msg,Clients)->
    orddict:map(fun (_ClientRef,ClientPid) -> ClientPid ! Msg end ,Clients )
        .

%% orddict 文档 http://learnyousomeerlang.com/a-short-visit-to-common-data-structures#key-value-stores
%% orddict是一种key-value 的处理方式，适于数量不是很大的情况,大约在75左右时最佳
init()->
    loop(_State=#state{clients=orddict:new(),events=orddict:new()})
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

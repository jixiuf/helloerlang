-module(gen_server_2).
-export([reply/2,loop/1,cast/2,call/2,start_link/2]).
-record(state,{module,value=[]}).


start_link(Module,Arg)->
    case Module:init([Arg])  of
        {ok,State}->
            Pid =spawn_link(?MODULE,loop,[#state{module=Module,value=State}]),
            {ok,Pid};
        {stop,Reason}->
            {stop,Reason};
        _ ->
            ignore
    end
        .

call(Name,Request)->
    Name!{call,Request,self()},
    receive
        {reply,Reply}->
            Reply;
        Other ->
            Other
    end
        .


cast(Name,Request)->
    spawn(fun()-> Name!{cast,Request} end),
    ok
        .
reply(Name,Reply)->
     Name!{reply,Reply}
        .
loop(S)->
    receive
        {call,Request,From}->
            Module = S#state.module,
            case  Module:handle_call(Request,From,S#state.value)  of
                {reply, Reply, State}->
                    From !{reply,Reply},
                    loop(S#state{value=State});
                {reply, Reply, State, _Timeout}->
                    %% From !{reply,Reply,Timeout},
                    %% TODO:
                    loop(S#state{value=State});
                {noreply, State}->
                    receive
                        {reply,Reply}->
                            From !{reply,Reply},
                            loop(S#state{value=State})
                    end;
                {noreply, State, Timeout} ->
                    %% TODO:
                    loop(S),
                    not_implement_yet;
                {stop, Reason, Reply, State}   ->
                    Module:terminate(Reason,State),
                    From !{reply,Reply}

            end;
        {cast,Requst} ->
            Module = S#state.module,
            case Module:handle_cast(Requst,S#state.value) of
                {noreply, State}->
                    loop(S#state{value=State});
                {noreply, State, Timeout}->
                    loop(S#state{value=State}),
                    not_implements_yet
                        ;
                {stop, Reason, State}->
                    Module:terminate(Reason,State)

            end


    end

        .
%%%    ==> {reply, Reply, State}
%%%
%%%
%%%
%%%

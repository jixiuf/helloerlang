-module(test_sys_2).
-export([test/0,send_msg/2,system_terminate/4,system_continue/3,write_debug/3,init/1,start_link/0]).


start_link()->
    proc_lib:spawn_link(?MODULE,init,[1])
    .

send_msg(Pid,Msg)->
    Pid!{app,Msg}
    .

init(State)->
    Debug=sys:debug_options([]),                %1111111111111111111111111111111111111111
    loop(State,Debug,self())
        .

loop(State,Debug,Parent)->
    receive
        {app,Msg}->
            sys:handle_debug(Debug,fun ?MODULE:write_debug/3,State,{in, Msg}), %222222222222
            loop(State,Debug,Parent);
        {system,From,Msg} ->                    %33333333333333333
            io:format("handing system msg... [From:~p,Msg:~p]~n",[From,Msg]) ,

            %% 这个函数不会返回。它会处理系统消息然后，如果需要进程继续执行则调用：
            %% Module:system_continue(Parent, Deb, State)
            %% 如果进程要终止则执行：
            %% Module:system_terminate(Reason, Parent, Deb, State)
            sys:handle_system_msg(Msg,From,Parent,?MODULE,Debug,State), %444444444
            loop(State,Debug,Parent)
    end
        .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_debug(Device,Event,Name)->
    io:format(Device,"name:~p,event:~p~n",[Name,Event])
    .
system_continue(Parent, Deb, State) ->
    io:format("system_continue is called [parent:~p~n,debugOpt:~p~n,state:~p]~n",[Parent,Deb,State]),
    loop(State,Deb,Parent).

system_terminate(Reason, _Parent, _Debug, _State) ->
    exit(Reason).
%% P=test_sys_2:start_link().
%% sys:statistics(P,true).
%% sys:trace(P,true).
%% test_sys_2:send_msg(P,hello).

test()->
    P=start_link(),
    sys:statistics(P,true),
    sys:trace(P,true),
    test_sys_2:send_msg(P,hello),
    sys:get_status(P)
        .

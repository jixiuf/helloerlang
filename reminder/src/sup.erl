-module(sup).
-compile([export_all]).

start(Mod,Args)->
    spawn(?MODULE,init,[Mod,Args]).
start_link(Mod,Args)->
    spawn_link(?MODULE,init,[Mod,Args]).
init(Mod,Args)->
    process_flag(trap_exit,true),
    loop(Mod,start_link,Args).

loop(Mod,Fun,Args)->
    Pid=apply(Mod,Fun,Args),
    receive
        {'EXIT',_From,shutdown}->
            debug:debug("super:","super die ,so all die..."),
            exit(shutdown);
        {'EXIT',Pid,_Reason}->
             io:format("super: ~p.~p() with Pid ~p die. and is restarting...",[Mod,Fun,Pid]),
            loop(Mod,Fun,Args)
    end.

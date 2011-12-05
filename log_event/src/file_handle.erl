-module(file_handle).
-compile([export_all]).
-behaviour(gen_event).

init(File)->
    {ok,Fd}=file:open(File,[write]),
    io:format("init file logger...~n",[]),
    {ok,Fd}.

handle_event(Msg,Fd)->
    io:format(Fd,"debug:~p~n",[Msg]),
    {ok,Fd}
        .

terminal(_Args,Fd)->
    file:close(Fd),
    ok.

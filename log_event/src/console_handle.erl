-module(console_handle).
-compile([export_all]).
-behaviour(gen_event).


init(_Args)->
    io:format("init...~n",[]),
    {ok,[]}.

handle_event(Msg,State)->
    io:format("debug:~p~n",[Msg]),
    {ok,State}
    .

terminal(_Args,State)->
    ok.

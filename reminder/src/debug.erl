-module(debug).
-compile([export_all]).

debug(Mod,Info)->
    io:format("~p:~p~n",[Mod,Info])
        .

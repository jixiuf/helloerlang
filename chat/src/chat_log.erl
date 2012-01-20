-module(chat_log).
-export([info/2,debug/2]).
-define(loglevel,debug).                        %available value: debug,info


info(Format,Params)->
    %% {ok,LogLevel}=application:get_env(loglevel),
    %% case LogLevel of
    case ?loglevel of
        info ->
            io:format("Info: "++Format,Params);
        debug ->
            io:format("Info: "++Format,Params);
        _Other ->
            io:format("",[])
    end
        .

debug(Format,Params)->
    %% {ok,LogLevel}=application:get_env(loglevel),
    %% case LogLevel of
    case ?loglevel of
        debug ->
            io:format("Debug: "++Format,Params);
        _Other ->
            io:format("",[])
    end
        .

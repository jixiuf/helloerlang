-module(game_demo).
-export([start/0,stop/1,start/2]).
-export([get_current_app_env/1,get_current_app_env/2]).

-include("include/base_header.hrl").
-include("include/debug.hrl").

start(normal,_Args)->
    server_sup:start_link().

stop(_State)->
    ?INFO2("~p:stop/1 is called~n",[?MODULE]),
    ok.

-spec get_current_app_env(Var) -> 'undefined' | Val when
   Var :: atom(),
   Val :: term().
get_current_app_env(Var)->
    case application:get_env(Var) of
        undefined->
            undefined;
        {ok,Val}->
            Val
    end.

-spec get_current_app_env(Var,DefaultVal) -> 'undefined' | Val when
   Var :: atom(),
   DefaultVal :: term(),
   Val :: term().
get_current_app_env(Var,DefaultVal)->
    case application:get_env(Var) of
        undefined->
            DefaultVal;
        {ok,Val}->
            Val
    end.

start()->
    application:start(?APP_NAME).

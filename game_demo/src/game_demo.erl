-module(game_demo).
-export([start/0,stop/1,start/2]).
-export([get_current_app_env/1,get_current_app_env/2]).
-define(DEFUALT_LISTEN_PORT,8888).

-include_lib("include/debug.hrl").

start(normal,_Args)->
    Port=get_current_app_env(listen_port,?DEFUALT_LISTEN_PORT),
    server_tcp:start_server(Port).

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
    application:start(?MODULE)
    .

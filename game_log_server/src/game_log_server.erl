-module(game_log_server).

-export([get_current_app_env/2,get_current_app_env/1,start/2,stop/1]).

start(normal,_Args)->
    io:format("game_log_server start/2 is running...~n",[]),
    emysql_center_sup:start_link();
start({failover,Node},_Args)->
    io:format("game_log_server{failover from ~p} start/2 is running...~n",[Node]),
    emysql_center_sup:start_link();
start({takeover,Node},_Args)->
    io:format("game_log_server{takeover from ~p} start/2 is running...~n",[Node]),
    emysql_center_sup:start_link().

stop(State)->
    io:format("game_log_server is stopped. with state:~p~n",[State]),
    emysql_center_sup:stop(),
    ok.

-spec get_current_app_env(Var) -> 'undefined' | {'ok', Val} when
   Var :: atom(),
   Val :: term().
get_current_app_env(Var)->
    application:get_env(?MODULE,Var).

-spec get_current_app_env(Var,DefaultVal) -> 'undefined' | {'ok', Val} when
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


-module(start_app).

-export([start/0,stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok;
        OtherError ->
            OtherError
    end.

start()->
    ensure_started(crypto),
    ensure_started(emysql),
    application:start(game_log_server).

stop()->
    true.

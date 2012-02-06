%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mochiweb_comet_chat.

-module(mochiweb_comet_chat).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the mochiweb_comet_chat server.
start() ->
    mochiweb_comet_chat_deps:ensure(),
    ensure_started(crypto),
    application:start(mochiweb_comet_chat).


%% @spec stop() -> ok
%% @doc Stop the mochiweb_comet_chat server.
stop() ->
    application:stop(mochiweb_comet_chat).

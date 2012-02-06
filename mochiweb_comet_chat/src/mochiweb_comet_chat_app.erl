%% @author Mochi Media <dev@mochimedia.com>
%% @copyright mochiweb_comet_chat Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the mochiweb_comet_chat application.

-module(mochiweb_comet_chat_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mochiweb_comet_chat.
start(_Type, _StartArgs) ->
    mochiweb_comet_chat_deps:ensure(),
    mochiweb_comet_chat_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mochiweb_comet_chat.
stop(_State) ->
    ok.

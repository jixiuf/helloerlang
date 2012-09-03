-module(server_handle).
-export([handle/1]).
%% 此模块中处理具体的业务逻辑
-include_lib("base_header.hrl").

handle(#c2s_protocol{header=?C2S_PROTOCOL_REGISTER,body=#echo{}=EchoRecord})->
    echo_handle:handle_echo(EchoRecord);
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% {S2CProtocol,Socket}; %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle(_Protocol)->
    throw(error)
    .

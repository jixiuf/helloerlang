-module(protocol_handle).

-export([handle/1]).
%% 此模块中处理具体的业务逻辑
-include("../include/base_header.hrl").
-include("../include/debug.hrl").

handle(C2SProtocol)->
    S2CProtocolList=handling(C2SProtocol),
    filter(S2CProtocolList)
    .

handling(#c2s_protocol{header=?C2S_PROTOCOL_ECHO,body=#c2s_echo{}=EchoRecord})->
    echo_handle:handle_echo(EchoRecord);
handling(_Protocol)->
    throw(error)
    .

%% 对某些协议进行合并，等处理
%% 比如 代码出可能多处有加铜币的操作
%% 对这样的协议可能需要合并成一条加铜币的协议处理
filter(S2CProtocolList)->
    S2CProtocolList
    .

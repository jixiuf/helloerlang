-module(server_encode).
-export([encode_server_error/0,encode/1]).

-compile([inline,[encode_server_error/0]]).

-include_lib("include/base_header.hrl").
-include_lib("include/debug.hrl").

encode(S2CProtocol)->
    try
        encoding(S2CProtocol)
    catch
        _:_->
            throw({error,encode})
    end.

encoding(S2CProtocol=#s2c_protocol{error_id=?S2C_ERROR_SUCCESS})->
    encoding_normal(S2CProtocol);
encoding(S2CProtocol=#s2c_protocol{error_id=_ErrorId}) ->
    encodeing_error(S2CProtocol)
        .

encoding_normal(#s2c_protocol{header=?S2C_PROTOCOL_ECHO,body=#s2c_echo{msg=Msg}})-> % 1:32 ,echo
    Bin = <<?S2C_PROTOCOL_ECHO:?S2C_PROTOCOL_LENGTH,?S2C_ERROR_SUCCESS:?S2C_ERROR_ID_LENGTH,Msg/binary>>,
    Bin;
encoding_normal(_S2CProtocol) ->
    throw({error,encode_unknow_protocol}).

%% 对于当errorId不为?S2C_ERROR_SUCCESS,一般情况下，只需将协议号及错误号传给客户端即可
%% 对于需要额外信息的，在此处加子句即可
encodeing_error(#s2c_protocol{error_id=ErrorId,header=ProtocolHeader})->
    <<ProtocolHeader:?S2C_PROTOCOL_LENGTH,ErrorId:?S2C_ERROR_ID_LENGTH>>.

encode_server_error()->
    <<?S2C_PROTOCOL_SERVER_ERROR:?S2C_PROTOCOL_LENGTH>>.

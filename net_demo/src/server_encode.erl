-module(server_encode).
-export([encode_server_error/0,encode/1]).

-compile([inline,[encode_server_error/0]]).

-include_lib("base_header.hrl").

encode(S2CProtocol)->
    try
        encoding(S2CProtocol)
    catch
        _:_->
            throw({error,encode})
    end.

encoding(#s2c_protocol{header=?S2C_PROTOCOL_ECHO,body=#echo{msg=Msg}})-> % 1:32 ,echo
    Bin = <<?S2C_PROTOCOL_ECHO:?S2C_PROTOCOL_LENGTH,Msg/binary>>,
    Bin;
encoding(_S2CProtocol) ->
    error.


encode_server_error()->
    <<?S2C_PROTOCOL_SERVER_ERROR:?S2C_PROTOCOL_LENGTH>>.

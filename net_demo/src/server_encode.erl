-module(server_encode).
-export([encode/1]).

-include_lib("base_header.hrl").

encode(S2CProtocol)->
    try
        encoding(S2CProtocol)
    catch
        _:_->
            throw({error,encode})
    end.

encoding(#s2c_protocol{header=?S2C_PROTOCOL_REGISTER,body=#echo{msg=Msg}})-> % 1:32 ,echo
    Bin = <<?S2C_PROTOCOL_REGISTER:?S2C_PROTOCOL_LENGTH,Msg/binary>>,
    Bin;
encoding(_S2CProtocol) ->
    error.

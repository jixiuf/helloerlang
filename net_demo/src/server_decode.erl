-module(server_decode).
-export([decode/1]).

-include_lib("base_header.hrl").

decode(Bin)->
    try
        decoding(Bin)
    catch
        _:_->
            throw({error,decode})
    end.

decoding(<<?C2S_PROTOCOL_REGISTER:?C2S_PROTOCOL_LENGTH,MsgBody/binary>>)-> % 1:32 ,echo
    io:format("server got echo msg from client:~p~n",[MsgBody]),
    EchoMsg= #echo{msg=MsgBody},
    Protocol=#c2s_protocol{header=?C2S_PROTOCOL_REGISTER,body=EchoMsg},
    {ok,Protocol}.

-module(server_decode).
-export([decode/1]).

-include_lib("../include/base_header.hrl").
-include_lib("../include/debug.hrl").

decode(Bin)->
    try
        decoding(Bin)
    catch
        _:_->
            throw({error,decode})
    end.

decoding(<<?C2S_PROTOCOL_ECHO:?C2S_PROTOCOL_LENGTH,MsgBin/binary>>)-> % 1:32 ,echo
    ?DEBUG2("server got echo msg from client:~p~n",[MsgBin]),
    {MsgBody,_OtherBin}=server_util:decode_str(MsgBin),
    EchoMsg= #c2s_echo{msg=MsgBody},
    Protocol=#c2s_protocol{header=?C2S_PROTOCOL_ECHO,body=EchoMsg},
    {ok,Protocol};
decoding(Bin)-> % 1:32 ,echo
    ?ERROR2("protocol decode error ,content:~p~n",[Bin]),
    throw({error,decode_unsupport_c2s_protocol})
.

-module(server_util).
-export([decode_str/1,encode_str/1]).
-export([build_resp/2,build_resp/3]).

-include_lib("../include/base_header.hrl").
-include_lib("../include/debug.hrl").

build_resp(S2cProtocol,Body)->
    #s2c_protocol{error_id=?S2C_ERROR_SUCCESS,header=S2cProtocol,body=Body}.

build_resp(ErrorId,S2cProtocol,Body)->
    #s2c_protocol{error_id=ErrorId,header=S2cProtocol,body=Body}.

%% server_util:encode_str(<<"asfd">>). == <<0,4,97,115,102,100>>
-spec encode_str(binary()|list())-> binary().
encode_str(Bin)when is_binary(Bin) ->
    Len=size(Bin),
    <<Len:?PROTOCOL_STRING_PREFIX,Bin/binary>>;
encode_str(Str) when is_list(Str)->
    Len=length(Str),
    Bin=list_to_binary(Str),
    <<Len:?PROTOCOL_STRING_PREFIX,Bin/binary>>.

%% server_util:decode_str(<<0,4,97,115,102,100>>)={<<"asfd">>,<<>>}
-spec decode_str(binary())->{binary(),binary()}.
decode_str(Bin) when is_binary(Bin)->
    <<Len:?PROTOCOL_STRING_PREFIX,OtherBin/binary>> = Bin,
    <<StrBin:Len/binary,OtherBin2/binary>> = OtherBin,
    {StrBin,OtherBin2}.

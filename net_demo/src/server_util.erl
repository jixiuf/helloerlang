-module(server_util).
-export([build_resp/2]).


-include_lib("base_header.hrl").

build_resp(S2cProtocol,Body)->
    #s2c_protocol{header=S2cProtocol,body=Body}.

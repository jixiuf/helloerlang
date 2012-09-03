-module(server_util).
-export([build_resp/2,build_resp/3]).


-include_lib("base_header.hrl").

build_resp(S2cProtocol,Body)->
    #s2c_protocol{error_id=?S2C_ERROR_SUCCESS,header=S2cProtocol,body=Body}.

build_resp(ErrorId,S2cProtocol,Body)->
    #s2c_protocol{error_id=ErrorId,header=S2cProtocol,body=Body}.

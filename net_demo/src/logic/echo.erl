-module(echo).
-export([cmd_echo/1]).


-include_lib("../base_header.hrl").

cmd_echo(Msg)->
    [server_util:build_resp(?S2C_PROTOCOL_ECHO,#s2c_echo{error_id=?S2C_ERROR_SUCCESS,msg=Msg})].

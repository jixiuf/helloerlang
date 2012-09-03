-module(echo_handle).
-export([handle_echo/1]).

-include_lib("../base_header.hrl").

handle_echo(#echo{msg=Msg})->
    try
        echo:cmd_echo(Msg)
    catch
        throw:ErrorId->
            [server_util:build_resp(?S2C_PROTOCOL_ECHO,#s2c_echo{error_id=ErrorId})];
        _:_->
            [server_util:build_resp(?S2C_PROTOCOL_ECHO,#s2c_echo{error_id=?S2C_ERROR_FAILED})]
    end.

-module(echo_handle).
-export([handle_echo/1]).

-include_lib("../base_header.hrl").

handle_echo(#c2s_echo{msg=Msg})->
    try
        echo:cmd_echo(Msg)
    catch
        throw:ErrorId->
            [server_util:build_resp(ErrorId,?S2C_PROTOCOL_ECHO,#s2c_echo{})];
        _:_->
            [server_util:build_resp(?S2C_ERROR_FAILED,?S2C_PROTOCOL_ECHO,#s2c_echo{})]
    end.

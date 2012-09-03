-module(echo_handle).
-include_lib("../base_header.hrl").

handle_echo()->
        S2CProtocol=#s2c_protocol{header=?S2C_PROTOCOL_ECHO,body=EchoMsg}.

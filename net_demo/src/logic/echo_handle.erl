-module(echo_handle).
handle_echo()->
        S2CProtocol=#s2c_protocol{header=?S2C_PROTOCOL_REGISTER,body=EchoMsg}.

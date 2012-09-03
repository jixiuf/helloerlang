-define(VERYFY(Value,ErrorId),case Value of false -> throw(ErrorId);Value-> Value end) .

-define(C2S_TCP_PACKET,2).
-define(C2S_PROTOCOL_LENGTH,16).
-define(C2S_PROTOCOL_ECHO,1).


-define(S2C_PROTOCOL_LENGTH,16).
-define(S2C_PROTOCOL_SERVER_ERROR,0).
-define(S2C_PROTOCOL_ECHO,1).


-record(c2s_protocol,{header,body}).
-record(s2c_protocol,{header,body}).

-record(echo,{msg}).

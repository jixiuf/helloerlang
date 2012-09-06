-define(APP_NAME,game_demo).
-define(VERYFY(Value,ErrorId),case Value of false -> throw(ErrorId);V-> V end) .


-define(C2S_TCP_PACKET,2).
-define(C2S_PROTOCOL_LENGTH,16).
-define(S2C_PROTOCOL_LENGTH,16).                %协议号的长度
-define(S2C_ERROR_ID_LENGTH,16).                %错误号的长度
-define(PROTOCOL_STRING_PREFIX,16).             %字符串，以 Len+Str的形式，先发长度后长内容


%% c2s 协议号
-define(C2S_PROTOCOL_ECHO,1).

%% s2c协议号
-define(S2C_PROTOCOL_SERVER_ERROR,0).
-define(S2C_PROTOCOL_ECHO,1).

%% errorid
-define(S2C_ERROR_SUCCESS,0).
-define(S2C_ERROR_FAILED,1).
%%  可以定义其他类型的errorid,


-record(c2s_protocol,{header,body}).
-record(s2c_protocol,{error_id=?S2C_ERROR_SUCCESS,header,body}).

-record(c2s_echo,{msg}).
-record(s2c_echo,{msg}).

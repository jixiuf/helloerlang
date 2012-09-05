-module(emc).
-behaviour(gen_server).

-define(SERVER_NAME_RELEASE,"127.0.0.1").
-define(SERVER_PORT_RELEASE,9180).

getServerAddress()->
    MemServerAddr =?SERVER_NAME_RELEASE,
    MemServerPort=?SERVER_PORT_RELEASE,
    {MemServerAddr,MemServerPort}
    .


set_value(Key,Value)->
    gen_server:call(?MODULE,{set ,to_list(Key),to_list(0),to_list(0),to_binary(Value)}).
set_value(Key,Flag,Value)->
    gen_server:call(?MODULE,{set ,to_list(Key),to_list(Flag),to_list(0),to_binary(Value)}).
set_value(Key,Flag,Exptime,Value) when is_integer(Flag),is_integer(Exptime)->
    gen_server:call(?MODULE,{set ,to_list(Key),to_list(Flag),to_list(Exptime),to_binary(Value)}).

add_value(Key,Value)->
    gen_server:call(?MODULE,{add ,to_list(Key),to_list(0),to_list(0),to_binary(Value)}).
add_value(Key,Flag,Value)->
    gen_server:call(?MODULE,{add ,to_list(Key),to_list(Flag),to_list(0),to_binary(Value)}).
add_value(Key,Flag,Exptime,Value) when is_integer(Flag),is_integer(Exptime)->
    gen_server:call(?MODULE,{add ,to_list(Key),to_list(Flag),to_list(Exptime),to_binary(Value)}).

replace_value(Key,Value)->
    gen_server:call(?MODULE,{replace ,to_list(Key),to_list(0),to_list(0),to_binary(Value)}).
replace_value(Key,Flag,Value)->
    gen_server:call(?MODULE,{replace ,to_list(Key),to_list(Flag),to_list(0),to_binary(Value)}).
replace_value(Key,Flag,Exptime,Value) when is_integer(Flag),is_integer(Exptime)->
    gen_server:call(?MODULE,{replace ,to_list(Key),to_list(Flag),to_list(Exptime),to_binary(Value)}).

to_list(Any)->
    "".
to_binary(A)->
    <<"">>.

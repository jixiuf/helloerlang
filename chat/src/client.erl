-module(client).
-export([close/1,echo/2,connect/2]).


connect(Host,Port) ->
    gen_tcp:connect(Host, Port, [binary])
        .

echo(Socket,Msg) when is_list(Msg)->            %Msg is string
    ok = gen_tcp:send(Socket,iolist_to_binary([util:encode_command("echo"),Msg]) )
        .
close(Socket)->
    ok = gen_tcp:close(Socket).

-module(client).

connect(Host,Port) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary]),
    ok = gen_tcp:send(Sock, <<4:32,"echo","hello">>),
    ok = gen_tcp:close(Sock).

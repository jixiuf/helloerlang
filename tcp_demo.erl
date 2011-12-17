-module(tcp_demo).
-compile(export_all).

start_server(Port) ->
    Pid = spawn_link(fun() ->
                             {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
                             spawn(fun() -> acceptor(Listen) end),
                             timer:sleep(infinity) %sleep ,避免前当进程退出 ，因为tcp socket 是与启动它的进程绑定的，如果进程死，socket关。
                     end),
    {ok, Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket), %
    spawn(fun() -> acceptor(ListenSocket) end),  %每次一个客户端连接上来，启用另一个进程继续兼听，而当前进程则用来处理刚连接进来的client
    handle(Socket).

%% Echoing back whatever was obtained
%% 与client 进行通信 ，这里仅仅是echo "erlang+msgfromClient"
handle(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            gen_tcp:send(Socket, "erlang "++Msg),
            handle(Socket)
    end.

%%tcp_demo:start_server(8888).

%% telnet 127.0.0.1 8888
%%hello
%% will got
%% erlang hello

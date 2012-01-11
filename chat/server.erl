-module(server).
-export([start_server/1]).


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
handle(ClientSocket) ->
    inet:setopts(ClientSocket, [{active, once}]),
    receive
        {tcp, ClientSocket,Bin}  when is_binary(Bin) ->
            case util:read_int32(Bin) of
                {Int,CommandBody} when is_integer(Int) and is_binary(CommandBody)->
                    io:format("handling command~n",[]) ,
                    case CommandBody of
                        <<Command:Int,Tail/binary>>->
                            handle_command(Command,Tail,ClientSocket);
                        _Other ->
                            handle(ClientSocket)
                    end

                        ;
                _Other ->
                    handle(ClientSocket)
            end ;
        _Other ->
            handle(ClientSocket)
    end.

handle_command("echo",CommandBody,ClientSocket)->
    io:format("helo~n",[]),
    gen_tcp:send(ClientSocket, <<4:32,"echo",CommandBody>>) %4 means length of "echo"
        .

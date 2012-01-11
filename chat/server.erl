-module(server).
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
handle(ClientSocket) ->
    inet:setopts(ClientSocket, [{active, once}]),
    receive
        {tcp, ClientSocket,Bin}  when is_binary(Bin) ->
            case read_int32(Bin) of
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
    gen_tcp:send(ClientSocket, CommandBody)
        .

%util function%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% read int32 from head of Bin data
%% return {intValue,TailBin}
read_int32(Bin)when is_binary(Bin)->
    case Bin of
        <<Int:32,T/binary>>->
            {Int,T} ;
        _O->
            io:format("messager format error!~n",[]),
            {error,message_format_error}
    end
        ;
read_int32(_NoneBin) ->
    {error,message_not_bin}.

%%int按网络字节流 转成binary
%%网络传输一般采用大端序big，也被称之为网络字节序，或网络序
%%而erlang 默认就是big
int32_2_binary(Int) when is_integer(Int)->
    <<Int:32>> ;
int32_2_binary(Bin) when is_binary(Bin) ->                          %若本就Bin ,直接返回
    Bin.

-module(server).
-export([start_server/1]).


start_server(Port) ->
    Pid = spawn_link(fun() ->
                             {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false},{packet ,4},{header,4}]),
                             spawn(fun() -> acceptor(Listen) end),
                             timer:sleep(infinity) %sleep ,避免前当进程退出 ，因为tcp socket 是与启动它的进程绑定的，如果进程死，socket关。
                     end),
    {ok, Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket), %
    spawn(fun() -> acceptor(ListenSocket) end),  %每次一个客户端连接上来，启用另一个进程继续兼听，而当前进程则用来处理刚连接进来的client
    handle(Socket).

%% Echoing back whatever was obtained
handle(ClientSocket) ->
    io:format("a client~n",[]),
    inet:setopts(ClientSocket, [{active, once}]),
    receive
        {tcp, ClientSocket,Bin}   ->
            io:format("~p~n",[Bin]) ,
            handle(ClientSocket) ;
        Other ->
            io:format("not bin ~p~n",[Other]),
            handle(ClientSocket)
    end.

%% handle_command(<<"echo">>,BinMsg,ClientSocket)->
%%     gen_tcp:send(ClientSocket,util:binary_concat(util:encode_command("msg"),BinMsg)) % means length of "echo" 4byte
%%         ;
%% handle_command(Command,CommandBody,_ClientSocket) ->
%%     io:format("other command:~p :~p~n",[Command,CommandBody]) .

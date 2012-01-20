-module(client).
-export([do_recv/1,close/1,echo/2,connect/2]).


connect(Host,Port) ->
    case gen_tcp:connect(Host, Port, [binary,{active,false},{packet,4}]) of
        {ok,ServerSocket}->
            Pid= spawn(?MODULE,do_recv,[ServerSocket]),
            register(?MODULE,Pid),              %pid of sending msg to server
            gen_tcp:controlling_process(ServerSocket, Pid),
            {ok,ServerSocket}
            %% timer:sleep(infinity)
                ;
        {error,Reason} ->
            {error,Reason}
    end
        .
do_recv(ServerSocket)->
    chat_log:debug("client do recving...~n",[]),
    inet:setopts(ServerSocket, [{active, once}]),
    receive
        {send,Bin,Socket}->
            chat_log:debug("client sending data to server ...~n",[]) ,
            gen_tcp:send(Socket,Bin),          %send Bin to ServerSocket
            do_recv(ServerSocket);
        {tcp, ServerSocket, Bin}->
            handle_command(Bin,ServerSocket),
            do_recv(ServerSocket);
        {tcp_closed, ServerSocket} ->
            chat_log:debug("server down!!!!!~n",[]),
            do_recv(ServerSocket) ;
        {tcp_error, ServerSocket, Reason} ->
            chat_log:debug("error ~p~n",[Reason]) ,
            {error,Reason}
    end
    %% case   gen_tcp:recv(ServerSocket,0) of
    %%     {send,Bin,Socket}->
    %%         io:format("client sending data to server ...~n",[]) ,
    %%         %% gen_tcp:send(Socket,Bin),          %send Bin to ServerSocket
    %%         do_recv(ServerSocket);
    %%     {ok,Bin}->
    %%         handle_command(Bin,ServerSocket),
    %%         do_recv(ServerSocket);
    %%     {error,Reason} ->
    %%         io:format("error ~p~n",[Reason]) ,
    %%         {error,Reason};
    %%     OtherMsg ->
    %%         io:format("OtherMsg:~p~n",[OtherMsg]),
    %%         do_recv(ServerSocket)
    %% end
        .
handle_command(<<1:32,EchoMsg/binary>>,_ServerSocket)-> %1:32 表示echo
    chat_log:info("client get msg from server and server said :~p~n",[binary_to_list(EchoMsg)])
    .

echo(Socket,Msg) when is_list(Msg)->            %Msg is string
    whereis(?MODULE) ! {send ,util:binary_concat(<<1:32>>,Msg),Socket},
    ok .

close(Socket)->
    ok = gen_tcp:close(Socket).

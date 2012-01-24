-module(client).
-export([password/2,register/1,user/2,do_recv/1,close/1,echo/2,connect/2]).


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
            chat_log:debug("client handle command ~n",[]),
            handle_command(Bin,ServerSocket),
            do_recv(ServerSocket);
        {tcp_closed, ServerSocket} ->
            chat_log:debug("tcp_closed!!!!!~n",[]),
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
        ;
handle_command(<<2:32,UserMsg/binary>>,_ServerSocket)-> %2:32 表示user
    case UserMsg of
        <<"ok">>->
            chat_log:debug("cmd:user executed successed~n",[]) ;
        <<"already_login">> ->
            chat_log:debug("user with the same name have alreadly login.~n",[])
    end

        ;
handle_command(<<3:32,Password/binary>>,_ServerSocket)-> %3:32 表示password
    case Password of
        <<"ok">>->
            chat_log:debug("cmd:password executed successed~n",[]) ;
        _Unknown_res ->
            chat_log:debug("unknown response.",[])
    end
        ;
handle_command(<<4:32,Register_Res/binary>>,_ServerSocket)-> %4:32 表示register
    case Register_Res of
        <<"ok">>->
            chat_log:debug("cmd:register executed successed~n",[]) ;
         <<"username_undefined">> ->
            chat_log:debug("username undefined.~n",[]);
          <<"password_undefined">> ->
            chat_log:debug("password undefined.~n",[]);
          <<"no_nickname">> ->
            chat_log:debug("warning:nickname undefined.~n",[])
    end
        ;
handle_command(Bin,_ServerSocket) ->
    io:format("other unhandled command ~p~n",[Bin])
        .

echo(Socket,Msg) when is_list(Msg)->            %Msg is string
    whereis(?MODULE) ! {send ,util:binary_concat(<<1:32>>,Msg),Socket},
    ok .

user(Socket,UserName) when is_list(UserName)->            %Msg is string
    whereis(?MODULE) ! {send ,util:binary_concat(<<2:32>>,UserName),Socket},
    ok .
password(Socket,Password) when is_list(Password)->            %Msg is string
    whereis(?MODULE) ! {send ,util:binary_concat(<<3:32>>,Password),Socket},
    ok .

register(Socket) ->            %Msg is string
    whereis(?MODULE) ! {send ,<<4:32>>,Socket},
    ok .

close(Socket)->
    ok = gen_tcp:close(Socket).

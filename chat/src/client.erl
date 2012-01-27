-module(client).
-export([msg/3,join/2,logout/1,login/1,nickname/2,password/2,register/1,user/2,do_recv/1,close/1,echo/2]).
-export([connect/2]).


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
            chat_log:debug("client tcp_closed!!!!!~n",[]),
            gen_tcp:close(ServerSocket),
            exit(normal)
            %% do_recv(ServerSocket)
             ;
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
        <<"username_cannot_start_with_#">> ->
            chat_log:debug("username_cannot_start_with_#~n",[]);
        <<"already_login">> ->
            chat_log:debug("user with the same name have alreadly login.~n",[])
    end
        ;
handle_command(<<5:32,NickName/binary>>,_ServerSocket)-> %5:32 表示nickname
    case NickName of
        <<"ok">>->
            chat_log:debug("cmd:nickname executed successed~n",[]) ;
        _ ->
            chat_log:debug("random message.",[])
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
            chat_log:debug("warning:nickname undefined.~n",[]);
        <<"user_already_registered",UserName/binary>> ->
            chat_log:debug("user[~p] already registered.~n",[UserName])
    end
        ;
handle_command(<<6:32,Login_Res/binary>>,_ServerSocket)-> %6:32 表示login
    case Login_Res of
        <<"ok">>->
            chat_log:debug("cmd:login executed successed~n",[]) ;
        <<"username_undefined">> ->
            chat_log:debug("username undefined.~n",[]);
        <<"password_undefined">> ->
            chat_log:debug("password undefined.~n",[]);
        <<"password_not_match">> ->
            chat_log:debug("password not match.~n",[]);
        <<"anonymous">> ->                      %匿名用户登录
            chat_log:debug("login as anonymous.~n",[]);
        <<"same_normal_user_already_logined",UserName/binary>> ->
            chat_log:debug("user[~p] same_normal_user_already_logined.~n",[UserName]);
        <<"same_anonymous_user_already_logined",UserName/binary>> ->
            chat_log:debug("user[~p] same_anonymous_user_already_logined.~n",[UserName])
    end
u        ;
handle_command(<<7:32,Logout_Res/binary>>,_ServerSocket)-> %7:32 表示logout
    case Logout_Res of
        <<"ok">>->
            chat_log:debug("logout successful.~n",[])
    end ;
handle_command(<<8:32,JoinInfo/binary>>,_ServerSocket)-> %8:32 表示join 加入聊天室
    case JoinInfo of
        <<"username_must_start_with_#">>->
            chat_log:debug("roomname_must_start_with_#~n",[]);
        <<"login_first">>->
            chat_log:debug("you should login first ,then run join command .~n",[]);
        _ ->
            ok
        end
     ;
handle_command(<<9:32,MsgInfo/binary>>,_ServerSocket)-> %9:32 表示向其他用户，或聊天室发送消息
    case MsgInfo of
        <<"dest_user_doesnot_logined">> ->
            chat_log:debug("cannot send msg to un logined user.~n",[]);
        <<"msg",Msg/binary>> ->
            chat_log:debug("msg~p~n",[Msg])
        end
     ;
handle_command(Bin,_ServerSocket) ->
    chat_log:info("other unhandled command ~p~n",[Bin])
        .

echo(Socket,Msg) when is_list(Msg)->            %Msg is string
    whereis(?MODULE) ! {send ,util:binary_concat([<<1:32>>,Msg]),Socket},
    ok .

user(Socket,UserName) when is_list(UserName)->            %UserName is string
    whereis(?MODULE) ! {send ,util:binary_concat([<<2:32>>,UserName]),Socket},
    ok .
nickname(Socket,NickName) when is_list(NickName)->            %NickName is string
    whereis(?MODULE) ! {send ,util:binary_concat([<<5:32>>,NickName]),Socket},
    ok .
password(Socket,Password) when is_list(Password)->            %password is string
    whereis(?MODULE) ! {send ,util:binary_concat([<<3:32>>,Password]),Socket},
    ok .

register(Socket) ->                             %
    whereis(?MODULE) ! {send ,<<4:32>>,Socket},
    ok .
login(Socket) ->                             %
    whereis(?MODULE) ! {send ,<<6:32>>,Socket},
    ok .
logout(Socket) ->                             %
    whereis(?MODULE) ! {send ,<<7:32>>,Socket},
    ok .
join(Socket,RoomName) when is_list(RoomName)->            %RoomName is string
    whereis(?MODULE) ! {send ,util:binary_concat([<<8:32>>,RoomName]),Socket},
    ok .
msg(Socket,RoomOrUserName,Msg) when is_list(RoomOrUserName) ->            %RoomName is string
    Len= length(RoomOrUserName),
    whereis(?MODULE) ! {send ,util:binary_concat([<<9:32,Len:32>>,RoomOrUserName,Msg]),Socket},
    ok .


close(Socket)->
    ok = gen_tcp:close(Socket).

-module(server).
-export([start_server/1]).
-include("records.hrl").


start_server(Port) ->
    prepare_db(),

    Pid = spawn_link(fun() ->
                             %%参照 http://erlangdisplay.iteye.com/blog/1012785
                             %% 假如，我定义 的消息格式为
                             %%[4字节数据长度][4字节消息类型,具体的消息体].
                             %%[4字节数据长度]由{packet,4}指定,
                             %% 表示发过来的消息，前4个字节用个表明消息体的长度,gen_tcp在接收数据的时候，先读取前4个字节,
                             %%发现此4字节的整数是100的话，继续读取直到长度为100 ,作为一个数据包读取结束。
                             %% 注意,这个过程是gen_tcp 自动执行的我们接收到的Bin 是100，而非104 ,即前4个字节已经自动切除掉了
                             %%而指定{header,4 }后，接收到的Bin [Byte1, Byte2,Byte3,Byte4 | Binary]即 4+96
                             %%而这4个字节用来表时消息类型
                             {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false},{packet ,4}]),
                             %% {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false},{packet ,4},{header,4}]),
                             %% {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
                             spawn(fun() -> acceptor(Listen) end),
                             timer:sleep(infinity) %sleep ,避免前当进程退出 ，因为tcp socket 是与启动它的进程绑定的，如果进程死，socket关。
                     end),
    {ok, Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket), %
    spawn(fun() -> acceptor(ListenSocket) end),  %每次一个客户端连接上来，启用另一个进程继续兼听，而当前进程则用来处理刚连接进来的client
    chat_log:debug("a new client is coming...~n",[]),
    handle(Socket).

%% Echoing back whatever was obtained
handle(ClientSocket) ->
    inet:setopts(ClientSocket, [{active, once}]),
    receive
        {tcp, ClientSocket,Bin}  when is_binary(Bin) ->
            chat_log:debug("server handle command...~n",[]),
            handle_command(Bin,ClientSocket) ,
            handle(ClientSocket) ;
        {tcp_closed,SocketSocket}->
            handle_tcp_closed(SocketSocket)
            ;
        {tcp_error, _Socket, Reason}->
            chat_log:debug("tcp_error with reason ~p~n:",[Reason]);
        Other ->
            io:format("other msg ~p~n",[Other]),
            handle(ClientSocket)
    end.

handle_command(<<1:32,MsgBody/binary>>,ClientSocket)-> % 1:32 ,echo
    chat_log:debug("server got echo msg from client:~p~n",[MsgBody]),
    gen_tcp:send(ClientSocket,<<1:32,MsgBody/binary>>) % means length of "echo" 4byte
        ;
%% login ,部分，传递用户名
handle_command(<<2:32,UserName/binary>>,ClientSocket)-> % 2:32 ,user
    chat_log:debug("Server got :cmd:user~p~n ",[UserName]),
    put(name,binary_to_list(UserName)),
    gen_tcp:send(ClientSocket,<<2:32,"ok">>)    %
        ;
handle_command(<<3:32,Password/binary>>,ClientSocket)-> % 3:32 ,Password
    chat_log:debug("Server got :cmd:password~p~n ",[Password]),
    put(password,binary_to_list(Password)),
    gen_tcp:send(ClientSocket,<<3:32,"ok">>);    %
handle_command(<<4:32,_/binary>>,ClientSocket)-> % 3:32 ,register
    chat_log:debug("Server got cmd:register ~n",[]),
    User = #user{name=get(name),password=get(password),nickname=get(nickname)},
    CheckUser = fun()->
                    if User#user.name =:= undefined
                       -> throw( <<"username_undefined">>);
                        User#user.password=:= undefined
                       -> throw( <<"password_undefined">>);
                       User#user.nickname =:= undefined
                       -> %% User#user{name=get(name)},
                          throw( <<"no_nickname">>); %use username as nickname if undefined,just a warning.
                       true -> <<"ok">>
                    end
            end,
    case catch CheckUser() of
        <<"ok">> ->
            Fun = fun()->
                          %% User=#user{name=UserName,password=Password,nickname=Nickname},
                          chat_log:debug("userName:~p,password:~p,nickname:~p~n",[User#user.name,User#user.password,User#user.nickname]),
                          mnesia:write(User)
                  end,
            case user_exists(User#user.name) of
                false->
                    mnesia:transaction(Fun) ,%save a registered user in mnesia db.
                    gen_tcp:send(ClientSocket,<<4:32,"ok">>);
                true ->
                    gen_tcp:send(ClientSocket,util:binary_concat(<<4:32,"user_already_registered">>,User#user.name))
            end;
        <<"no_nickname">> ->
            NewUser=User#user{nickname=get(name)}, %use name as the nickname
            Fun = fun()->
                          chat_log:debug("userName:~p,password:~p,nickname:~p~n",[NewUser#user.name,NewUser#user.password,NewUser#user.nickname]),
                          mnesia:write(NewUser)
                  end,
            case user_exists(User#user.name) of
                false->
                    mnesia:transaction(Fun) ; %save a registered user in mnesia db.
                true ->
                    gen_tcp:send(ClientSocket,util:binary_concat(<<4:32,"user_already_registered">>,NewUser#user.name))
            end,
            gen_tcp:send(ClientSocket,<<4:32,"no_nickname">>)    ; %still send "no_nickname" to client ,so that client can do something.
        BinMsg ->
            gen_tcp:send(ClientSocket,util:binary_concat(<<4:32>>,BinMsg))    %
    end
;
handle_command(<<5:32,Nickname/binary>>,ClientSocket)-> % 5:32 ,nickname
    chat_log:debug("server got nickname[~p] msg from client.~n",[Nickname]),
    put(nickname,binary_to_list(Nickname)),
    gen_tcp:send(ClientSocket,<<5:32,"ok">>) % means length of "echo" 4byte

        .
handle_tcp_closed(ClientSocket)->
    chat_log:debug(" tcp_closed:~p!~n",[ClientSocket])
        .
prepare_db()->
    %% mnesia:create_schema([node()]),             %若无此句则，仅在内存中有效，但是此句只能运行一次，所以。。。。
    mnesia:start(),
    mnesia:create_table(user,[{type,set},{attributes,record_info(fields ,user)}]), %set 不允许重复数据
    mnesia:create_table(room,[{type,set},{attributes,record_info(fields ,room)}]), %set 不允许重复数据
    mnesia:create_table(activated_user,[{type,set},{attributes,record_info(fields ,activated_user)}]) %set 不允许重复数据
    .
%% return true or false,whether user exists in db
user_exists(UserName)->
    Fun = fun()->
                  mnesia:read(user,UserName)  %参数{Tab,Key},似乎这个Key 是-record的第一个属性
          end,
    {atomic,Result}= mnesia:transaction(Fun),
    case length(Result) of
        0->
            false;
        _ ->
            true
    end.

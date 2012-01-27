-module(server).
-export([room_p/1,start_server/1]).
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
    put(user_login_p ,false),                    %当前进程标记为未登录状态(此时此进程刚刚启动，客户端未来得及运行任务命令)
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
        {logout,_FromPid}->                                %服务器端强迫客户端下线，正常用户的登录强迫同名匿名用户下线
            chat_log:debug("server force client do logout~n",[]) ,
            gen_tcp:close(ClientSocket),
            exit(normal) ;
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
    chat_log:debug("Server got :cmd:user~p~n",[UserName]),
    case  string:chr(binary_to_list(UserName),$#)  of %判断name 是不是以
        1->
            gen_tcp:send(ClientSocket,<<2:32,"username_cannot_start_with_#">>) ;   %
        _ ->
            put(name,binary_to_list(UserName)),
            %% DONE:注销已登录用户(如果有)
            case get(user_login_p) of
                true ->
                    do_logout(get(name));
                anonymous ->
                    do_logout(get(name));
                undefined ->
                    ok;
                false->
                    ok
                end,
            put(user_login_p,false),                 %如果客户端传过来新的用户名，将此前用户标记为未登录
            %%如果已经有一个用户登录了，又运行了一次 cmd:user ,则相当于注销已登录用户
            gen_tcp:send(ClientSocket,<<2:32,"ok">>)    %
    end ;
handle_command(<<3:32,Password/binary>>,ClientSocket)-> % 3:32 ,Password
    chat_log:debug("Server got :cmd:password~p~n",[Password]),
    put(password,binary_to_list(Password)),
    gen_tcp:send(ClientSocket,<<3:32,"ok">>);    %
handle_command(<<4:32,_/binary>>,ClientSocket)-> % 4:32 ,register
    chat_log:debug("Server got cmd:register ~n",[]),
    User = #users{name=get(name),password=get(password),nickname=get(nickname)},
    CheckUser = fun()->
                        case User#users.name of
                            undefined->
                                throw( <<"username_undefined">>);
                            _ ->
                                ok
                        end,
                        case User#users.password of
                            undefined->
                                throw( <<"password_undefined">>);
                            "" ->
                                throw( <<"password_undefined">>);
                            _->
                                ok
                        end,
                        case User#users.nickname  of
                            undefined->
                                throw( <<"no_nickname">>); %use username as nickname if undefined,just a warning.
                            _ ->
                                <<"ok">>
                        end
                end,
    case catch CheckUser() of
        <<"ok">> ->
            Fun = fun()->
                          %% User=#users{name=UserName,password=Password,nickname=Nickname},
                          chat_log:debug("userName:~p,password:~p,nickname:~p~n",[User#users.name,User#users.password,User#users.nickname]),
                          mnesia:write(User)
                  end,
            case user_exists(User#users.name) of
                false->
                    mnesia:transaction(Fun) ,%save a registered user in mnesia db.
                    gen_tcp:send(ClientSocket,<<4:32,"ok">>);
                true ->
                    gen_tcp:send(ClientSocket,util:binary_concat([<<4:32,"user_already_registered">>,User#users.name]))
            end;
        <<"no_nickname">> ->
            NewUser=User#users{nickname=get(name)}, %use name as the nickname
            Fun = fun()->
                          chat_log:debug("userName:~p,password:~p,nickname:~p~n",[NewUser#users.name,NewUser#users.password,NewUser#users.nickname]),
                          mnesia:write(NewUser)
                  end,
            case user_exists(User#users.name) of
                false->
                    mnesia:transaction(Fun) ; %save a registered user in mnesia db.
                true ->
                    gen_tcp:send(ClientSocket,util:binary_concat([<<4:32,"user_already_registered">>,NewUser#users.name]))
            end,
            gen_tcp:send(ClientSocket,<<4:32,"no_nickname">>)    ; %still send "no_nickname" to client ,so that client can do something.
        BinMsg ->
            gen_tcp:send(ClientSocket,util:binary_concat([<<4:32>>,BinMsg]))    %
    end
        ;
handle_command(<<5:32,Nickname/binary>>,ClientSocket)-> % 5:32 ,nickname
    chat_log:debug("server got nickname[~p] msg from client.~n",[Nickname]),
    put(nickname,binary_to_list(Nickname)),
    gen_tcp:send(ClientSocket,<<5:32,"ok">>) % means length of "echo" 4byte
        ;
handle_command(<<6:32,_/binary>>,ClientSocket) -> %login
    chat_log:debug("Server got cmd:login ~n",[]),
    User = #users{name=get(name),password=get(password),nickname=get(nickname)},
    CheckUser = fun()->
                        case User#users.name of
                            undefined->
                                throw( <<"username_undefined">>);
                            _ ->
                                ok
                        end,
                        case User#users.password of
                            undefined->
                                throw( <<"password_undefined">>);
                            "" ->
                                throw( <<"anonymous">>);
                            _->
                                <<"ok">>
                        end
                end,
    case catch CheckUser() of
        <<"ok">> ->                             %正常用户登录，
            Fun = fun()->
                          Activated_User= #activated_user{name=User#users.name,registered=true,client_pid=self(),client_socket_id=ClientSocket,update_time=now()},
                          mnesia:write(Activated_User)
                  end,
            case user_login_p(User#users.name) of %此用户名是否已经登录
                false->
                    case user_login(User#users.name,User#users.password) of
                        true->                        %用户名密码正确
                            mnesia:transaction(Fun) ,%login
                            gen_tcp:send(ClientSocket,<<6:32,"ok">>),
                            put(user_login_p,true);
                        false->
                            gen_tcp:send(ClientSocket,<<6:32,"password_not_match">>)
                    end ;
                true ->
                    gen_tcp:send(ClientSocket,util:binary_concat([<<6:32,"same_normal_user_already_logined">>,User#users.name]));
                anonymous ->                    %如果已经有一个匿名登录用户，
                    %% gen_tcp:send(ClientSocket,<<6:32,"anonymous">>)
                    case  query_activated_user(User#users.name) of %首先查出同名已经登录的匿名用户名相关信息
                        [{activated_user,_UserName,_Registered_flg,Pid,_Client_socket_id,_Time}] ->
                            Pid!{logout,self()}, %for client logout
                            %% TODO:maybe need update ,when new tables are used
                            case user_login(User#users.name,User#users.password) of
                                true->                        %用户名密码正确
                                    %% mnesia:transaction(Fun) ,%login
                                    mnesia:transaction(Fun) ,%activated_user 是set 类型,write 时，会冲掉同key的记录
                                    gen_tcp:send(ClientSocket,<<6:32,"ok">>),
                                    put(user_login_p,true);
                                false->
                                    gen_tcp:send(ClientSocket,<<6:32,"password_not_match">>)
                            end
                    end
            end;
        <<"anonymous">> ->                             %匿名用户登录，
            Fun = fun()->
                          Activated_User= #activated_user{name=User#users.name,registered=false,client_pid=self(),client_socket_id=ClientSocket,update_time=now()},
                          mnesia:write(Activated_User)
                  end,
            case user_login_p(User#users.name) of
                false->                         %没有同名用户登录
                    mnesia:transaction(Fun) ,%login
                    gen_tcp:send(ClientSocket,<<6:32,"anonymous">>),
                    put(user_login_p,anonymous);
                true ->                         %有同名用户正常登录,则此次匿名登录失败
                    gen_tcp:send(ClientSocket,util:binary_concat([<<6:32,"same_normal_user_already_logined">>,User#users.name]));
                anonymous ->                    %有一个同名匿名登录用户,则此次匿名登录失败
                    gen_tcp:send(ClientSocket,util:binary_concat([<<6:32,"same_anonymous_user_already_logined">>,User#users.name]))
            end;
        BinMsg ->
            gen_tcp:send(ClientSocket,util:binary_concat([<<6:32>>,BinMsg]))    %
    end;
handle_command(<<7:32,_Logout/binary>>,ClientSocket)-> % 7:32 ,logout
    chat_log:debug("server:logout ~p....~n",[get(name)]),
    gen_tcp:send(ClientSocket,<<7:32,"ok">>), %
    do_logout(get(name)),
    gen_tcp:close(ClientSocket), %
    exit(normal);
handle_command(<<8:32,RoomNameBin/binary>>,ClientSocket)-> % 8:32 ,join room
    RoomName = binary_to_list(RoomNameBin),
    chat_log:debug("server:join room ~p ....~n",[RoomName]),
    case  get(user_login_p) of
        false ->
            gen_tcp:send(ClientSocket,<<8:32,"login_first">>)    %you should login first ,then join
                ;
        _Logined ->
            case  string:chr(RoomName,$#)  of %判断name 是不是以#开头
                1->
                    do_join(get(name),RoomName,ClientSocket);
                _ ->
                    gen_tcp:send(ClientSocket,<<8:32,"roomname_must_start_with_#">>)    %
            end
    end;
handle_command(<<9:32,MsgInfo/binary>>,ClientSocket)-> % 9:32 ,msg
    {RoomOrUserName,Msg} =util:split_binary_by_head_int_value(MsgInfo),
        case room_p(RoomOrUserName) of
            true->
                do_msg_room(RoomOrUserName,Msg,ClientSocket);
            false ->
                do_msg_user(RoomOrUserName,Msg,ClientSocket)
        end
 .
%% send msg to all user in roomname
do_msg_room(RoomName,Msg,_ClientSocket)->
    io:format("room name:~p said:~p~n",[RoomName,Msg])
    .
%% send msg to user
do_msg_user(UserName,Msg,ClientSocket)->
        case query_activated_user(binary_to_list(UserName)) of
            []->
                    gen_tcp:send(ClientSocket,<<9:32,"dest_user_doesnot_logined">>)    %
                ;
            [{activated_user,_Name,_Registered,_Client_pid,Client_socket_id,_Update_time }] ->
                gen_tcp:send(Client_socket_id,util:binary_concat([<<9:32>>,Msg]))    %
            end


    .

do_logout(UserName)->
    io:format("server do logout clean up job.~n",[]),
        %% TODO:db clean up,tcp clean up
    Fun = fun()->
                  mnesia:delete({activated_user,UserName})
          end,
    mnesia:transaction(Fun)
    .
%% 如果roomname 已经存在，则直接将用户加入到聊天室，否则，先创建聊天室，后加入
do_join(UserName,RoomName,_ClientSocket)->
    io:format("server do join .~n",[]),
    case  room_exists(RoomName) of
        true->
            ok;
        false ->                                %先创建聊天室，(若不存在)
            create_room(RoomName,UserName)
    end,
    insert_room_user(RoomName,UserName)
        .

handle_tcp_closed(ClientSocket)->
    chat_log:debug(" tcp_closed:~p!~n",[ClientSocket])
        .
prepare_db()->
    %% mnesia:create_schema([node()]),             %若无此句则，仅在内存中有效，但是此句只能运行一次，所以。。。。
    mnesia:start(),
    mnesia:create_table(users,[{type,set},{attributes,record_info(fields ,users)}]), %set 不允许重复数据
    mnesia:create_table(room,[{type,set},{attributes,record_info(fields ,room)}]), %set 不允许重复数据
    mnesia:create_table(room_user,[{type,bag},{attributes,record_info(fields ,room_user)}]) ,%bag 允许重复数据
    mnesia:create_table(activated_user,[{type,set},{attributes,record_info(fields ,activated_user)}]) %set 不允许重复数据
    .
%% return true or false,whether user exists in db
user_exists(UserName)->
    Fun = fun()->
                  mnesia:read(users,UserName)  %参数{Tab,Key},似乎这个Key 是-record的第一个属性
          end,
    {atomic,Result}= mnesia:transaction(Fun),
    case length(Result) of
        0->                                     %
            false;
        _ ->
            true
    end.

%% 创建聊天室
create_room(RoomName,UserName)->
    Desc = "room name :"++ RoomName++ " create by:" ++UserName++ " @:"++util:time_to_string( calendar:local_time()),
    Room = #room{room_name=RoomName,desc=Desc,creater=UserName,create_time=now(),update_time=now()},
    Fun = fun()->
                  mnesia:write(Room)
          end,
    mnesia:transaction(Fun)
    .

insert_room_user(RoomName,UserName)->
    RoomUser = #room_user{room_name=RoomName,user_name=UserName},
    Fun = fun()->
                  mnesia:write(RoomUser)
          end,
    mnesia:transaction(Fun)
.
%% check RoomName 是否合法( 以#  开头)
room_p(RoomName) when is_list(RoomName)->
    case  string:chr(RoomName,$#)  of %判断name 是不是以#开头
        1->                                           %是
            true;
        _ ->
            false
    end ;
room_p(RoomName)when is_binary(RoomName) ->
    <<Char:1/binary,_Tail/binary>> = RoomName,
    case Char of
        <<"#">>->
            true;
        _ ->
            false
    end.

room_exists(RoomName)->
    Fun = fun()->
                  mnesia:read(room,RoomName)  %参数{Tab,Key},似乎这个Key 是-record的第一个属性
          end,
    {atomic,Result}= mnesia:transaction(Fun),
    case length(Result) of
        0->                                     %
            false;
        _ ->
            true
    end.
%% user 是否已login
%% @return
%% true -->logined
%% false --> not logined
%% anonymous -->anonymous login
user_login_p(UserName)->
    Fun = fun()->
                  mnesia:read(activated_user,UserName)  %参数{Tab,Key},似乎这个Key 是-record的第一个属性
          end,
    {atomic,Result}= mnesia:transaction(Fun),
    case length(Result) of
        0->
            false;
        _ ->                                    %normal 1
            [{activated_user,_,Registered_flg,_,_,_}|_] = Result,
            case Registered_flg of
                true->
                    true;                       %normal login
                false ->
                    anonymous                   %anonymous login
            end
    end.
%% 检查用户名密码是否正确
%% check password ok ,
%% @return : true or false
user_login(UserName,Password)->
    Fun = fun()->                                                        %select name from users where name=? and password=?
                  MatchPattern=  #users{_='_',name='$1',password='$2' }, %相当于将name绑定到$1, 上，下文中Guard,Result 可以引用之，
                  Guard=[{'==','$1', UserName},{'==','$2', Password}],                   %$1 == UserName 作为 判断条件
                  Result=['$1'],                            %结果，只取$1作为返回值
                  mnesia:select(users,[{MatchPattern,Guard,Result}])
          end,
    {atomic,Result}=mnesia:transaction(Fun),
    case Result of
        []->
            false;
        [UserName] ->
            true
        end

    .
%% [{activated_user,"jixf",true,<0.81.0>,{1327,497303,181846}}]
%% []
query_activated_user(UserName)->
    Fun = fun()->
                  mnesia:read(activated_user,UserName)  %参数{Tab,Key},似乎这个Key 是-record的第一个属性
          end,
    {atomic,Result}= mnesia:transaction(Fun),
    Result
        .

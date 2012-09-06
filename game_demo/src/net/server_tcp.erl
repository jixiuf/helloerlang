-module(server_tcp).
-export([start_server/2,start_server/1]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

-include("../include/base_header.hrl").
-include("../include/debug.hrl").

-define(RECBUF_SIZE, 8192).
%%  The backlog value defines the maximum length that the queue of pending connections may grow to.
-define(BACKLOG,5).                             %
-define(NODELAY,true).
-define(TCP_OPTS, [binary, {active, false},{reuseaddr,true},{packet ,?C2S_TCP_PACKET},{recbuf, ?RECBUF_SIZE},{backlog,?BACKLOG},{nodelay,?NODELAY}]).
-record(state,{listener,port,tcp_opts}).

start_server(Port) ->
    start_server(Port,?TCP_OPTS).

start_server(Port,TcpOpts) ->
    start_link(Port,TcpOpts).

start_link(Port,TcpOpts)->
    gen_server:start_link(?MODULE,#state{port=Port,tcp_opts=TcpOpts},[]).

init(S=#state{port=Port,tcp_opts=TcpOpts})->
    %%参照 http://erlangdisplay.iteye.com/blog/1012785
    %% 假如，我定义 的消息格式为
    %%[4字节数据长度][4字节消息类型,具体的消息体].
    %%[4字节数据长度]由{packet,4}指定,
    %% 表示发过来的消息，前4个字节用个表明消息体的长度,gen_tcp在接收数据的时候，先读取前4个字节,
    %%发现此4字节的整数是100的话，继续读取直到长度为100 ,作为一个数据包读取结束。
    %% 注意,这个过程是gen_tcp 自动执行的我们接收到的Bin 是100，而非104 ,即前4个字节已经自动切除掉了
    %%而指定{header,4 }后，接收到的Bin [Byte1, Byte2,Byte3,Byte4 | Binary]即 4+96
    %%而这4个字节用来表时消息类型
    {ok, Listen} = gen_tcp:listen(Port,TcpOpts),
    %% {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false},{packet ,4},{header,4}]),
    %% {ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
    process_flag(trap_exit,true),
    From=self(),
    spawn_link(fun() -> acceptor(Listen,From) end),
    %% timer:sleep(infinity), %sleep ,避免前当进程退出 ，因为tcp socket 是与启动它的进程绑定的，如果进程死，socket关。 %%
    {ok, S#state{listener=Listen}}
        .

handle_call(Request,_From,State)->
    ?DEBUG2("random handle_call msg~p~n",[Request]) ,
    {noreply, State}
        .

handle_cast(accepted,State=#state{listener=ListenSocket})->
    From =self(),
    spawn_link(fun() -> acceptor(ListenSocket,From) end),  %每次一个客户端连接上来，启用另一个进程继续兼听，而当前进程则用来处理刚连接进来的client
    {noreply, State};
handle_cast(Request,State)->
    ?DEBUG2("random handle_cast msg~p~n",[Request]) ,
    {noreply, State} .


handle_info({'EXIT',_FromPid,Reason},State)->
    ?DEBUG2("clien pid exit with reason:~p~n",[Reason]),
    %% 客户端所在进程当掉，
    %% 在那里进行业务逻辑相关清理工作 %%
    {noreply, State};
handle_info(Info,State)->
    ?DEBUG2("random handle_info msg~p~n",[Info]) ,
    {noreply, State}.

terminate(Reason,_State)->
    ?DEBUG2("terminated with reason~p~n",[Reason]) ,
    ok .

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .

acceptor(ListenSocket,From) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket), %
    ?DEBUG2("a new client is coming...~n",[]),
    gen_server:cast(From,accepted),
    handle(Socket,From).

handle(ClientSocket,From) ->
    inet:setopts(ClientSocket, [{active, once}]),
    receive
        {tcp, ClientSocket,Bin}  when is_binary(Bin) ->
            ?DEBUG2("server handle data...~n",[]),
            case handle_data(Bin,ClientSocket)  of
                ok->ok;
                {error, Reason}-> self()!{exit,self(),Reason}
            end,
            handle(ClientSocket,From);
        {tcp_closed,Socket}->
            handle_tcp_closed(Socket);
        {tcp_error, Socket, Reason}->
            ?DEBUG2("tcp_error with reason ~p~n:",[Reason]),
            handle_tcp_closed(Socket);
        {exit,_FromPid,Reason}->                                %服务器端强迫客户端下线，正常用户的登录强迫同名匿名用户下线
            ?DEBUG2("exit with reason~p~n",[Reason]) ,
            handle_tcp_closed(ClientSocket),
            exit(normal) ;
        Other ->
            ?DEBUG2("handle random msg ~p~n",[Other]),
            handle(ClientSocket,From)
    end.

handle_tcp_closed(ClientSocket)->
    ?DEBUG2(" tcp_closed:~p!~n",[ClientSocket]),
    %% do some clean job here  ,when client socket lost
    gen_tcp:close(ClientSocket),                 %try to close client socket,
    %% exit(),退出， 督程收到信号，在那里进行业务逻辑相关清理工作
    exit(tcp_closed).

handle_data(Bin,ClientSocket) ->
    EncodeData=
        try
            {ok,C2SProtocol}=server_decode:decode(Bin) ,
            S2CProtocols=server_handle:handle(C2SProtocol),
            [server_encode:encode(S2CProtocol)||S2CProtocol<-S2CProtocols]
        catch
            throw:Msg->
                ?DEBUG2("~p:handle_data/2 error with reason:~p~n",[?MODULE,Msg]) ,
                server_encode:encode_server_error();
            error:ErrorId ->
                ?DEBUG2("~p:handle_data/2 error with reason:~p~n",[?MODULE,ErrorId]) ,
                server_encode:encode_server_error()
        end,
    gen_tcp:send(ClientSocket,EncodeData).

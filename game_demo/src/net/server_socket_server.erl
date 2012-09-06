-module(server_socket_server).

-export([start_link/0,start_link/2]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

-include("../include/base_header.hrl").
-include("../include/debug.hrl").

-define(RECBUF_SIZE, 8192).
%%  The backlog value defines the maximum length that the queue of pending connections may grow to.
-define(DEFUALT_LISTEN_PORT,8888).
-define(BACKLOG,5).                             %
-define(NODELAY,true).
-define(TCP_OPTS, [binary, {active, false},{reuseaddr,true},{packet ,?C2S_TCP_PACKET},
                   {recbuf, ?RECBUF_SIZE},{backlog,?BACKLOG},{nodelay,?NODELAY}]).
-record(state,{listener,port,tcp_opts}).

start_link() ->
    Port=?APP_NAME:get_current_app_env(listen_port,?DEFUALT_LISTEN_PORT),
    start_link(Port,?TCP_OPTS).

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
    gen_server:cast(self(),accepted),
    {ok, S#state{listener=Listen}}
        .

handle_call(Request,_From,State)->
    ?DEBUG2("random handle_call msg~p~n",[Request]) ,
    {noreply, State}
        .

handle_cast(accepted,State=#state{listener=ListenSocket})->
    From =self(),
    %每次一个客户端连接上来，启用另一个进程继续兼听，而当前进程则用来处理刚连接进来的client
    server_socket:start_link(ListenSocket,From),
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

-module(server_socket_server).

-export([status/0,start_link/0,start_link/2]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

-include("../include/base_header.hrl").
-include("../include/debug.hrl").

-define(DEFUALT_LISTEN_PORT,8888).

-define(RECBUF_SIZE, 8192).
%%  The backlog value defines the maximum length that the queue of pending connections may grow to.
-define(BACKLOG,128).                             %
-define(NODELAY,true).
-define(TCP_OPTS, [binary, {active, false},{reuseaddr,true},{packet ,?C2S_TCP_PACKET},
                   {recbuf, ?RECBUF_SIZE},{backlog,?BACKLOG},{nodelay,?NODELAY}]).
-record(state,{listener,port,tcp_opts,
               max=2048,                   %最大允许连接数
               acceptor_pool_size=100,           %连接池大小
               active_sockets=0,                %已连接客户端数
               acceptor_pool=sets:new()}).      %已经放入连接池等待客户端连接的 acceptor

start_link() ->
    Port=?APP_NAME:get_current_app_env(listen_port,?DEFUALT_LISTEN_PORT),
    start_link(Port,?TCP_OPTS).

start_link(Port,TcpOpts)->
    MaxConn=?APP_NAME:get_current_app_env(max_conn_num,2048),
    AcceptorPoolSize=?APP_NAME:get_current_app_env(acceptor_pool_size,100),
    gen_server:start_link({local,?MODULE},?MODULE,
                          #state{port=Port,
                                 acceptor_pool_size=AcceptorPoolSize,
                                 max=MaxConn,
                                 tcp_opts=TcpOpts},[]).
status()->
    State= gen_server:call(?MODULE,status),
    #state{port=Port,
           tcp_opts=TcpOpts,
           max=Max,
           acceptor_pool_size=AcceptorPoolSize,
           active_sockets=ActiveSockets,
           acceptor_pool=AcceptorPool}=State,
    ?DEBUG2("~nport=~p,~n tcp_opts=~p,~n max_connection=~p,~n acceptor_pool_size=~p~n active_sockets=~p~n acceptor_pool=~p~n",
            [Port,TcpOpts,Max,AcceptorPoolSize,ActiveSockets,sets:to_list(AcceptorPool)])
        .
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
    {ok,new_acceptor_pool(Listen,self(),S)}.

handle_call(status,_From,State)->
    {reply,State,State};
handle_call(Request,_From,State)->
    ?DEBUG2("random handle_call msg~p~n",[Request]) ,
    {reply,ok, State}.

handle_cast({accepted,ClientPid},State=#state{active_sockets=ActiveSockets})->
    From =self(),
    %%每次一个客户端连接上来，启用另一个进程继续兼听，而当前进程则用来处理刚连接进来的client
    %% {ok,AcceptedPid}=server_socket:start_link(ListenSocket,From), %%
    State1 = State#state{active_sockets=1 + ActiveSockets},
    NewState=recycle_acceptor(ClientPid,From,State1),
    {noreply, NewState};
handle_cast(Request,State)->
    ?DEBUG2("random handle_cast msg~p~n",[Request]) ,
    {noreply, State} .


handle_info({'EXIT', Pid, normal}, State) ->
    ?DEBUG("clien pid exit with reason:normal~n"),
    spawn(fun()->handle_process_exit(Pid)end),
    From=self(),
    {noreply, recycle_acceptor(Pid, From,State)};
handle_info({'EXIT', Pid, Reason},
            State=#state{acceptor_pool=Pool}) ->
    case sets:is_element(Pid, Pool) of
        true ->
            %% If there was an unexpected error accepting, log and sleep.
            error_logger:error_report({?MODULE, ?LINE,
                                       {acceptor_error, Reason}}),
            timer:sleep(100);
        false ->
            ok
    end,
    spawn(fun()->handle_process_exit(Pid)end),
    {noreply, recycle_acceptor(Pid,self(), State)};
handle_info(Info,State)->
    ?DEBUG2("random handle_info msg~p~n",[Info]) ,
    {noreply, State}.

terminate(Reason,_State)->
    ?DEBUG2("terminated with reason~p~n",[Reason]) ,
    ok .

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .


new_acceptor_pool(Listen,From,State=#state{acceptor_pool=Pool,acceptor_pool_size=Size}) ->
    F = fun (_, S) ->
                {ok,Pid}=server_socket:start_link(Listen,From),
                sets:add_element(Pid, S)
        end,
    Pool1 = lists:foldl(F, Pool, lists:seq(1, Size)),
    State#state{acceptor_pool=Pool1,listener=Listen}.

recycle_acceptor(Pid,From, State=#state{
                             max=Max,
                             acceptor_pool=Pool,
                             listener=Listen,
                             active_sockets=ActiveSockets}) ->
    ?DEBUG2("recycle_acceptor,pid=~p~n",[Pid]),
    case sets:is_element(Pid, Pool) of
        true ->                                 %一个新的连接建立，从pool中用一新的acceptor替换之
            case sets:size(Pool)+ActiveSockets>Max  of
                true->
                    %%如果大于最大连接上限，则不向pool
                    %% 中再加新连接，此时将会使pool的实际大小变小，故当有连接断开时,
                    %% 需要判断需不需要补回短少的部分
                    Pool1 =sets:del_element(Pid, Pool),
                    State#state{acceptor_pool=Pool1};
                false ->
                    {ok,NewAcceptorPid}=server_socket:start_link(Listen,From),
                    Pool1 = sets:add_element(NewAcceptorPid, sets:del_element(Pid, Pool)),
                    State#state{acceptor_pool=Pool1}
                end;
        false ->                                %一个连接断开，
            case sets:size(Pool)+ActiveSockets==Max  of
                true->  %如果大于最大连接上限，则向不pool中再加新连接
                    {ok,NewAcceptorPid}=server_socket:start_link(Listen,From),
                    Pool1 = sets:add_element(NewAcceptorPid, Pool),
                    State#state{acceptor_pool=Pool1,active_sockets=ActiveSockets - 1};
                false ->
                    State#state{active_sockets=ActiveSockets - 1}
            end

    end.

handle_process_exit(ClientPid)->
    %% 客户端连接 process exit ,此处进行业务逻辑数据的清理,
    %% 主要是清除一些内存中的在线玩家数据，
    %% TODO:
    _ClientSocket =server_socket:get_socket(ClientPid)
    %% do some clean job
    .

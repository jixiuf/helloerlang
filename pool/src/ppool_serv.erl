-module(ppool_serv).
-behaviour(gen_server).
-compile([export_all]).

%% ppool_worker_sup 与此module ppool_serv 为ppool_super的子节点。
%% 不同之处在于此module ppool_serv,由ppool_super直接静态启动。
%% 而ppool_worker_sup在此module 中，
%% {ok,Pid}= supervisor:start_child(Super,Args), 此处Super为ppool_super
%% 动态启动，启动过程中，将super 设为ppool_super
%%不同时使用静态启动的原因：若同时启动，则两子节点无法感知彼此的存在。
%% 无法交换数据。
%%一个ppool_super是一个pool, 而ppool_super的父节点是ppool_supersuper
%% 即此程序中可以存在多个pool ，一个 pool 的死活与另一个pool 不相干 ，
%%故在ppool_supersuper中使用one_for_one 启动ppool_super.
%%而ppool_super的两个子节点，ppool_worker_sup与ppool_serv却是同生共死才能完成一个
%% pool 的功能 。
%%故ppool_worker_sup与ppool_serv 进行关联启动， 可以理解了。
-define(SPEC(MFA),
        {ppool_worker_sup,                           %id
         {ppool_worker_sup,start_link,[MFA]},
         permanent , 10000, supervisor, [ppool_worker_sup]
        }
       ).
%% 此记录，sup 是与此 ppool_serv相关联的兄弟节点ppool_worker_sup
%%refs 记录了此pool中进行的ref ,queue 是池中的进程
%% ,如果poolsize >0 ,直接run ,否则加到queue中
-record(state,{poolsize=0,sup,refs,queue=queue:new()}).

%% ppool_serv 由 ppool_super启动，故ParentPid 为 ppool_super
start_link(PoolName,PoolSize,MFA,ParentPid)                                                          ->
    io:format("ppool_serv is starting ...   ",[]),
    io:format("a pool named: ~p is adding with size ~p... ~n",[PoolName,PoolSize]),
    gen_server:start_link({local,PoolName},?MODULE,[PoolSize,MFA,ParentPid],[])
        .

%% ppool_serv 由 ppool_super启动，故ParentPid 为 ppool_super
start(PoolName,PoolSize,MFA,ParentPid)                                                               ->
    io:format("ppool_serv is starting aaa ...~n",[]),
    gen_server:start({local,PoolName},?MODULE,[PoolSize,MFA,ParentPid],[])
        .

%% ppool_serv 由 ppool_super启动，故ParentPid 为 ppool_super
init([PoolSize,MFA,ParentPid])                                                                         ->
    io:format("ppool_serv initing...~n",[]),
    %%给自已发一条消息，然后接收到消息后用 supervisor:start_child(Super,Args), 启动ppool_worker_sup
    %%不在此处直接写supervisor:start_child(Super,Args) ,是因为,此函数是init() ,即运行到此行代码时，
    %% init() 函数还未结束 ，即还未初始化完成 ，此节点的父节点，即Super会一直等待此节点初始化结束 。
    %% supervisor:start_child也是同步的。若在此得调用 ，则会阻塞，
    %% 故通过异步消息的方式完成启动兄弟进程ppool_worker_sup
    self()!{start_worker_sup,ParentPid,MFA},
    {ok,#state{poolsize=PoolSize,refs=gb_sets:empty()}}
        .
%% run ,如果还有空间，直接run. 若无，则返回一个noalloc 的reply
handle_call({run ,Args}, _From, State=#state{poolsize=PoolSize,sup=Super,refs=Refs}) when PoolSize>0 ->
    io:format("runing ... ~n",[]),
    {ok,Pid}= supervisor:start_child(Super,Args),%此处的Super 是ppool_worker_sup模块
    Ref = erlang:monitor(process,Pid),
    {reply,{ok,Pid},State#state{poolsize=PoolSize-1 ,refs= gb_sets:add(Ref,Refs) }};
handle_call({run ,_Args}, _From, State=#state{poolsize=PoolSize}) when PoolSize=< 0 ->
    {reply,noalloc,State};


%% sync  ,如果还有空间，直接run. 若无 ,进信息加入队列，等待空闲时run
handle_call({sync ,Args}, _From, State=#state{poolsize=PoolSize,sup=Super,refs=Refs}) when PoolSize>0->
    io:format("runing ... ~n",[]),
    {ok,Pid}= supervisor:start_child(Super,Args), %此处的Super 是ppool_worker_sup模块
    Ref = erlang:monitor(process,Pid),
    {reply,{ok,Pid},State#state{poolsize=PoolSize-1 ,refs= gb_sets:add(Ref,Refs) }};
handle_call({sync ,Args}, From, State=#state{poolsize=PoolSize,queue=Queue}) when PoolSize=< 0 ->
    {noreply,State#state{queue= queue:in({From,Args},Queue)}};

handle_call(stop,_From,State) ->
    io:format("stoping...~n",[]),
    {stop,normal,ok,State};
handle_call(_Msg,_From,State) ->
    {noreply,State}
        .

handle_cast({async,Args},State=#state{poolsize=PoolSize,sup=Super,refs=Refs})when PoolSize>0->
    io:format("async queue~n",[]),
    {ok,Pid}= supervisor:start_child(Super,Args),%此处的Super 是ppool_worker_sup模块
    Ref = erlang:monitor(process,Pid),
    {reply,{ok,Pid},State#state{poolsize=PoolSize-1 ,refs= gb_sets:add(Ref,Refs) }};
handle_cast({async,Args},State=#state{poolsize=PoolSize,queue=Queue})when PoolSize =< 0 ->
    io:format("up to queue size ,add to queue~n",[]),
    {noreply,State#state{queue= queue:in(Args,Queue)}};
handle_cast(Msg,State) ->
    io:format("other cast info ~p~n",[Msg]),
    {noreply,State}
        .

%% 启动与此ppool_serv相关联的的ppool_worker_sup,将进pid 存到#state.sup 中。
handle_info({start_worker_sup,ParentPid,MFA},State=#state{})->
    io:format("start_worker_sup starting ppool_worker_sup...  ,~n",[]),
    {ok,Pid}= supervisor:start_child(ParentPid,?SPEC(MFA)),
    {noreply,State#state{sup=Pid}};
handle_info({'DOWN',Ref,process,Pid,Reason}, State=#state{poolsize=PoolSize,sup=Super,refs=Refs}) ->
    case gb_sets:is_member(Ref,Refs) of
        true->
            io:format("a process down ,~n",[]),
            handle_down_work(Ref,State);
        false->
            io:format("other processes I don't care .~n",[]),
            {noreply,State}
    end
        ;
handle_info(Msg,State) ->
    io:format("Unexpected Msg:~p~n",[Msg]),
    {noreply,State}
        .

terminate(normal, State) ->
    io:format("ppool_serv stopped!~n",[]),
    ok;
terminate(Reason, State) ->
    io:format("ppool_serv stopped with reason :~p!~n",[Reason]),
    ok.
%% 当一个池中的进程down ,检查队列中有没有进程 ，有则启动之
handle_down_work(Ref,State=#state{poolsize=PoolSize,sup=Super,refs=Refs,queue=Queue})->
    io:format("handing down work~n",[]),
    case queue:out(State#state.queue) of
        {{value,{From,Args}},NewQueue}->
            io:format("a sync process will running~n",[]),
            {ok,Pid}= supervisor:start_child(Super,Args),
            NewRef = erlang:monitor(process,Pid),
            NewRefs =gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
            gen_server:reply(From, {ok,Pid}),
            {noreply,State#state{ refs= NewRefs,queue=NewQueue }}
                ;
        {{value,Args},NewQueue} ->
            io:format("a async process will running~n",[]),
            {ok,Pid}= supervisor:start_child(Super,Args),
            NewRef = erlang:monitor(process,Pid),
            NewRefs =gb_sets:insert(NewRef, gb_sets:delete(Ref,Refs)),
            {noreply,State#state{ refs= NewRefs,queue=NewQueue }}
                ;
        {empty,_} ->
            io:format("no process in queue~n",[]) ,
            {noreply,State#state{ poolsize=PoolSize+1 ,refs=gb_sets:delete(Ref,Refs)}}
    end

        .
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%interface
run(Name,Args)->
    gen_server:call(Name,{run,Args})
        .
sync_queue(Name,Args)->
    gen_server:call(Name,{sync,Args},infinity)
        .
async_queue(Name,Args)->
    gen_server:cast(Name,{async,Args})
        .

stop(Name)->
    gen_server:call(Name,stop).
%% end of interface

%% Local Variables:
%% coding: utf-8
%% End:

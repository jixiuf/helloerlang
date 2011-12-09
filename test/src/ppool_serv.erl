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
-record(state,{poolsize=0,sup,refs,queue=queue:new()}).

%% ppool_serv 由 ppool_super启动，故ParentPid 为 ppool_super
start_link(PoolName,PoolSize,MFA,ParentPid)->
    io:format("ppool_serv is starting ...~n",[]),
    supervisor:start_link({local,PoolName},?MODULE,[PoolSize,MFA,ParentPid])
        .

%% ppool_serv 由 ppool_super启动，故ParentPid 为 ppool_super
start(PoolName,PoolSize,MFA,ParentPid)->
    io:format("ppool_serv is starting ...~n",[]),
    supervisor:start({local,PoolName},?MODULE,[PoolSize,MFA,ParentPid])
        .

%% ppool_serv 由 ppool_super启动，故ParentPid 为 ppool_super
init(PoolSize,MFA,ParentPid)->
    io:format("ppool_serv initing...~n",[]),
    %%给自已发一条消息，然后接收到消息后用 supervisor:start_child(Super,Args), 启动ppool_worker_sup
    %%不在此处直接写supervisor:start_child(Super,Args) ,是因为,此函数是init() ,即运行到此行代码时，
    %% init() 函数还未结束 ，即还未初始化完成 ，此节点的父节点，即Super会一直等待此节点初始化结束 。
    %% supervisor:start_child也是同步的。若在此得调用 ，则会阻塞，
    %% 故通过异步消息的方式完成启动兄弟进程ppool_worker_sup
    self()!{start_worker_sup,ParentPid,MFA},
    {ok,#state{poolsize=PoolSize,refs=gb_sets:empty()}}
        .

handle_call({run ,Args}, _From, State=#state{poolsize=PoolSize,sup=Super,refs=Refs}) when PoolSize>0->
    io:format("runing ... ~n",[]),
    {ok,Pid}= supervisor:start_child(Super,Args),
    Ref = erlang:monitor(process,Pid),
    {reply,{ok,Pid},State#state{poolsize=PoolSize-1 ,refs= gb_sets:add(Ref,Refs) }};
handle_call({run ,_Args}, _From, State=#state{poolsize=PoolSize}) when PoolSize=< 0 ->
    {reply,noalloc,State}.


handle_info({start_worker_sup,ParentPid,MFA},State=#state{})->
    {ok,Pid}= supervisor:start_child(ParentPid,?SPEC(MFA)),
    {noreply,State#state{sup=Pid}};
handle_info(Msg,_State=#state{}) ->
    io:format("Unexpected Msg:~p~n",[Msg]) .


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

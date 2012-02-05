-module(tcp_load_test).
-export([timer/2,connect/1,recv/1,start/2,start/1]).
%% 启用10000 个tcp 连接
%% tcp_load_test:start(10000).
%% tcp_load_test:start("http://localhost:8080/time",10).

%% 测试可以打开的tcp 接接数量
%% Tuning the Linux Kernel for many tcp connections
%% Save yourself some time and tune the kernel tcp settings before testing with
%% lots of connections, or your test will fail and you’ll see lots of Out of
%% socket memory messages (and if you are masquerading, nf_conntrack: table full,
%% dropping packet.)
%% $ cat /etc/sysctl.conf
%% # General gigabit tuning:
%% net.core.rmem_max = 16777216
%% net.core.wmem_max = 16777216
%% net.ipv4.tcp_rmem = 4096 87380 16777216
%% net.ipv4.tcp_wmem = 4096 65536 16777216
%% net.ipv4.tcp_syncookies = 1
%% # this gives the kernel more memory for tcp
%% # which you need with many (100k+) open socket connections
%% net.ipv4.tcp_mem = 50576   64768   98152
%% net.core.netdev_max_backlog = 2500
%% # I was also masquerading the port comet was on, you might not need this
%% net.ipv4.netfilter.ip_conntrack_max = 1048576

%% sysctl -p

%% ulimit -n 显示最大#        - nofile - max number of open files
%% ulimit -n 99999 设置

%%#cat /etc/security/limits.conf
%% jixiuf hard    nofile 99999
%% jixiuf soft    nofile 99999


start(MaxConnectCount)->
    start("http://localhost:8080/time",MaxConnectCount)
    .
start(Url,MaxConnectCount)->
    inets:start(),
    Pid = spawn(?MODULE,recv,[{0,0,0}]),
    register(recv,Pid),

    %%每隔两秒钟取一次状态
    spawn(?MODULE,timer,[2000,whereis(recv)]),
    loop(Url,MaxConnectCount)
        .

%% tcp 连接测试
loop(Url,0)->
    connect(Url++"?id=0");
    %% spawn(?MODULE,connect,[Url++"?id=0"]);
loop(Url,ConnectionCount)->
    connect(Url++"?id=" ++ integer_to_list(ConnectionCount)),
    %% spawn(?MODULE ,connect,[Url++"?id=" ++ integer_to_list(ConnectionCount)]),
    loop(Url,ConnectionCount-1)
    .
%% state 记录了，当胆有几个活动连接，几个closed的连接，共接到多少个chunks(所有连接的总和)
recv(State)->
    process_flag(trap_exit,true),
    {Active , Closed , Chunks} = State,

  receive
        state -> io :format("Stats: ~w\n " ,[State])
        after 0 -> noop
    end ,

    receive
        { http,{ _Ref ,stream_start,_X}} ->  recv({Active+1 ,Closed ,Chunks}) ;
        { http,{ _Ref ,stream,_X}} ->          recv({Active , Closed , Chunks+1}) ;
        { http,{ _Ref ,stream_end,_X}} ->  recv({Active-1 , Closed+1 , Chunks}) ;
        { http,{ _Ref ,{ error,Why}}} ->
            io :format("Closed: ~w\n " ,[Why]) ,
            recv({Active-1 , Closed+1 , Chunks}) ;
        { loadurl, Url} ->
            httpc :request( get, {Url , []} , [],[{ sync, false} , { stream, self} ,  { body_format, binary}]) ,
            recv(State)
    end.

connect(Url)->
    io:format("~p~n",[Url]),
    whereis(recv)!{loadurl,Url}
        .

%% T ,time ,Who:recv Pid
timer(T , Who) ->
    receive
    after T ->
            Who ! state
    end ,
    timer(T , Who) .

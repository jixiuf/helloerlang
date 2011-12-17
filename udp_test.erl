-module(udp_test).
%% 在本机8780 open udp连接,并准备接收数据
%% {ok,Server}=gen_udp:open(8789,[binary,{active,false}]).
%% gen_udp:recv(Server,0).

%% 另外启动一个erl ,
%% 在8791 端口 open 一个udp 连接，作为客户端以便与server通信
%% {ok,Client}=gen_udp:open(8791).
%% 通过Client 与远程127.0.0.1:8789  通信，发送一条信息 "hello"
%% gen_udp:send(Client,{127,0,0,1},8789,"helo").


%% tcp
%%% server
%% {ok, ListenSocket} = gen_tcp:listen(8091, [{active, true}, binary]).
%% {ok, AcceptSocket} = gen_tcp:accept(ListenSocket).
%% {ok, AcceptSocket} = gen_tcp:accept(ListenSocket, 2000).

%% client
%% {ok, Socket} = gen_tcp:connect({127,0,0,1}, 8091, [binary, {active,true}]).
%% gen_tcp:send(Socket, "Hey there first shell!").

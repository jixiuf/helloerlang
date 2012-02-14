-module(udp_test).
-export([send/1,listen/0]).
%% 这是我的广播地址，
%%  {ok,Ifaddrs}= inet:getifaddrs(),可以返回相关的信息，
%% 但是此处我写死的
-define(BROADADDR,"172.20.68.255").

%%使用udp_test:listen() 开如接听，
%% send 用于广播,向9999
%% udp_test:send("hello").
%% udp_test:send(<<"hello">>).

listen()->
    case gen_udp:open(9999) of
        {ok, Socket}->
            loop(Socket);
        {error, Reason} ->
            io:format("error:~p~n",[Reason])

    end.

loop(Socket)->
    receive
        Any ->
            io:format("received:~p~n",[Any]),
            loop(Socket)
    end
        .




send(IoList)->
    %% eth0在linux 代表网卡
    %% {ok,Ifaddrs}= inet:getifaddrs(),
    case  gen_udp:open(8888,[{broadcast,true}]) of
        {ok, Socket}->
            gen_udp:send(Socket,?BROADADDR,9999,IoList),
            gen_udp:close(Socket);
        {error, Reason} ->
            io:format("error:~p~n",[Reason])

    end

        .

-module(group_leader_io).
-export([print/0]).


print()->
    io:format("hello t1 ,group leader:~p~n",[erlang:group_leader()]) ,
    erlang:group_leader(whereis(user),self()),
    io:format("hello t2 ,group leader:~p~n",[erlang:group_leader()])
        .

%% erl -sname t1
%% erl -sname t2
%% 在t1中执行:
%% net_kernel:connect_node('t2@jf.org').
%%  rpc:call('t2@jf.org', group_leader_io, print, []).
%% 会看到t2中输出hello world


%% 另外，想把shell的输出重定向的文件里，可以这样
%% {ok, Log} = file:open("log", [write]),
%% erlang:group_leader(Log, self()).

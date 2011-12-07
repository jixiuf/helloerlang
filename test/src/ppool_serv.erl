-module(ppool_serv).
-behaviour(supervisor).
-compile([export_all]).

start_link(PoolName,PoolSize,MFA,ParentPid)->
    io:format("ppool_serv is starting ...~n",[]),
    supervisor:start_link({local,pool_serv},?MODULE,[])
.

init([])->
    io:format("ppool_serv initing...~n",[]),
    MaxTime=3600,
    MaxRestart=1,
    {ok,{{one_for_all,MaxRestart,MaxTime},[]}}  %此supervisor中目前无任何进程
        .
%% Local Variables:
%% coding: utf-8
%% End:

-module(ppool_worker_sup).
-behaviour(supervisor).
-compile([export_all]).

start_link(PoolName,PoolSize,MFA)->
    io:format("ppool_worker_sup is starting ...~n",[]),
    supervisor:start_link({local,pool_super},?MODULE,[PoolName,PoolSize,MFA])
.

init([PoolName,PoolSize,MFA])->
    io:format("ppool_worker_sup initing...~n",[]),
    MaxTime=3600,
    MaxRestart=1,
    {ok,{{one_for_all,MaxRestart,MaxTime},
         [{ppool_serv,                           %id
           {ppool_serv,start_link,[PoolName,PoolSize,MFA,self()]},
           permanent, 5000, worker, [ppool_serv]
          }
         ]}}
        .
%% Local Variables:
%% coding: utf-8
%% End:

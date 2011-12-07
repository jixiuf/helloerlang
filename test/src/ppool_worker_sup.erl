-module(ppool_worker_sup).
-behaviour(supervisor).
-compile([export_all]).

start_link(MFA={_,_,_})->
    io:format("ppool_worker_sup is starting ...~n",[]),
    supervisor:start_link({local,pool_worker_super},?MODULE,[MFA])
.

init({M,F,A})->
    io:format("ppool_worker_sup initing...~n",[]),
    MaxTime=3600,
    MaxRestart=1,
    {ok,{{simple_one_for_one,MaxRestart,MaxTime},
         [{pool_worker_super,                           %id
           {ppool_serv,start_link,[PoolName,PoolSize,MFA,self()]},
           permanent, 5000, worker, [ppool_serv]
          }
         ]}}
        .
%% Local Variables:
%% coding: utf-8
%% End:

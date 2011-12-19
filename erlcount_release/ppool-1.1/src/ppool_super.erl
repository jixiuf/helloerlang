-module(ppool_super).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/3]).

start_link(PoolName,PoolSize,MFA)->
    ppool_log:debug("ppool_super is starting ...~n",[]),
    supervisor:start_link({local,pool_super},?MODULE,[PoolName,PoolSize,MFA])
.

init([PoolName,PoolSize,MFA])->
    ppool_log:debug("ppool_super initing...~n",[]),
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

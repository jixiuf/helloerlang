-module(ppool_worker_sup).
-behaviour(supervisor).
-export([init/1]).
-export([start_link/1]).

start_link(MFA={_,_,_})->
    io:format("ppool_worker_sup is starting ...~n",[]),
    supervisor:start_link({local,pool_worker_super},?MODULE,MFA)
.

init({M,F,A})->
    io:format("ppool_worker_sup initing...~n",[]),
    MaxTime=3600,
    MaxRestart=1,
    {ok,{{simple_one_for_one,MaxRestart,MaxTime},
         [{pool_worker,                           %id
           {M,F,A},
           temporary, 5000, worker, [M]
          }
         ]}}
        .
%% Local Variables:
%% coding: utf-8
%% End:

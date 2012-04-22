-module(emysql_center_sup).
-behaviour(supervisor).
-export([stop/0,start_link/0,init/1]).

start_link()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init([])->
    MaxRestart=3,
    MaxTime=5000,
    {ok,{{one_for_one,MaxRestart,MaxTime},
         [ %using spec snippet for childspec
           {emysql_center,                           %id
            {emysql_center,start_link,[]},
            transient, infinity, worker, [emysql_center]
           }
         ]}}.

stop()->
    emysql_center:stop().

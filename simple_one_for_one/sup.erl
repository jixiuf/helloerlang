-module(sup).
-behaviour(supervisor).
-export([start_link/0,init/1]).

start_link()->
    supervisor:start_link(?MODULE,[])
        .
init([])->
    io:format("initing supervisor~n",[]),
    {ok,{{simple_one_for_one,5,3000},
         [{workerid,                           %id
           {worker,start_link,[]},
           temporary, 5000, worker, [worker]
          } %using spec snippet for childspec
         ]}}
        .

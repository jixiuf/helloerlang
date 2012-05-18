-module(hello_sup).
-vsn("0.2").
%% 升级到0.2之后也对world_gen进行监督

-behaviour(supervisor).

-export([start_link/0,init/1]).

start_link()->
    supervisor:start_link(?MODULE,[])
        .
init([])->
    {ok,{{one_for_one,5,3000},
         [ %using spec snippet for childspec
           {hello_gen,                           %id
            {hello_gen,start_link,[]},
            permanent, 3000, worker, [hello_gen]
           },
           {world_gen,                           %id
            {world_gen,start_link,[]},
            permanent, 3000, worker, [world_gen]
           }
         ]}}
        .

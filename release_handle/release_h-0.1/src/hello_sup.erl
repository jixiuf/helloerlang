-module(hello_sup).
-vsn("0.1").

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
           }
         ]}}
        .

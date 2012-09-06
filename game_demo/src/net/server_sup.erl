-module(server_sup).

-behaviour(supervisor).
-export([start_link/0,init/1]).

%% -include("../include/base_header.hrl"). %%
-include("../include/debug.hrl").

start_link()->
    supervisor:start_link({local,?MODULE},?MODULE,[]).

init([])->
    MaxRestart=3,
    MaxTime=5000,
    {ok,{{one_for_one,MaxRestart,MaxTime},
         [ %using spec snippet for childspec
           {server_socket_monitor,                           %id
            {server_socket_monitor,start_link,[]},
            permanent, infinity, worker, [server_socket_monitor]
           }]}}.

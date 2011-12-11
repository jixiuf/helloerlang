-module(ppool_supersuper).
-behaviour(supervisor).
-export([init/1]).
-export([start_pool/3,stop_pool/1]).
-export([start_link/0]).

start_link()->
    io:format("ppool_supersuper is starting ...~n",[]),
    supervisor:start_link({local,ppool},?MODULE,[])
.

init([])->
    io:format("ppool_supersuper initing...~n",[]),
    MaxTime=3600,
    MaxRestart=6,
    {ok,{{one_for_one,MaxRestart,MaxTime},[]}}  %此supervisor中目前无任何进程
        .
%% don't need any more . otp will take care of it .
%% take a look at ppool.erl
%% stop()->                                        % stoping ppool_supersuper itself.
%%     io:format("pool_supersuper is stoping...~n",[]),
%%     case whereis(pool_supersuper) of
%%         P when is_pid(P) ->
%%             exit(P,kill);
%%         _ -> ok
%%     end
%%         .


start_pool(PoolName,PoolSize,MFA4worker)->
    io:format("staring pool: ~p~n",[PoolName]),
    ChildSpec={PoolName,                          %一个id标识
               {ppool_super,start_link,[PoolName,PoolSize,MFA4worker]},           %此行，批示一个fun  : {Mod ,fun, param}
               permanent, infinity, supervisor, [ppool_super]},
    supervisor:start_child(pool_supersuper,ChildSpec)
        .

stop_pool(PoolName)->
    io:format("stoping pool ~p~n",[PoolName]),
    supervisor:terminate_child(ppool_super,PoolName),
    supervisor:delete_child(pool_supersuper,PoolName)
        .

-module(ppool).
-compile([export_all]).
%% ppool API

start_link()->
ppool_supersuper:start_link()
    .
stop_pool(PoolName)->
ppool_supersuper:stop_pool(PoolName).

start_pool(PoolName,PoolSize,MFA4worker)->
    ppool_supersuper:start_pool(PoolName,PoolSize,MFA4worker).

stop()->
    ppool_supersuper:stop().

run(Name,Args)->
    ppool_serv:run(Name,Args)
        .
sync_queue(Name,Args)->
    ppool_serv:sync_queue(Name,Args)
        .
async_queue(Name,Args)->
    ppool_serv:async_queue(Name,Args)
        .

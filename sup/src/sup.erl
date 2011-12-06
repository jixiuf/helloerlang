-module(sup).
-compile([export_all]).

start1()->
    start_link(type1)
    .

start2()->
    start_link(type2)
        .

start3()->
    start_link(type3)
    .

start_link(Type)->
    supervisor:start_link({local ,?MODULE},?MODULE,Type)
    .

init(type1)->
    init({one_for_one,3,5000});                 %当一个worker die,只重启此死worker
init(type2) ->
    init({one_for_all,3,5000});                 %当一个worker die ,重启所有的worker 及其子supervisor
init(type3) ->
    %% X works alone, but Y depends on X and Z depends on both
    %% A starts B, which starts C, which starts D, 当B 死掉时，只需要重启C D,C死，只重启D
    %% if a process dies, all the ones that were started after it (depend on it) get restarted
    init({rest_for_all,3,5000});

init({RestartStrategy,MaxRestart,MaxTime}) ->
    {ok,{{RestartStrategy,MaxRestart,MaxTime},
         [
          {worker1_id,                          %一个id标识
           {worker1, start_link, []},           %此行，批示一个fun  : {Mod ,fun, param}
           permanent, 1000, worker, [worker1]}, %A permanent process should always be restarted, no matter what.
          {worker2_id,
           {worker2, start_link, []},
           %%transient 介于permanent 与temporary之间，此进程若正常die(normal) 则不重启，否则 重启
           %% 适用于一次性完成任务后，就不再启用的场合。
           transient, 1000, worker, [worker2]},
          {worker3_id,
           {worker3, start_link, []},
           temporary, 1000, worker, [worker3]}  %a temporary process is a process that should never be restarted.
         ]
        }}.

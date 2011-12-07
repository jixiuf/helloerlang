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
%% 除以上三种外，还有一种simple_one_for_one

init({RestartStrategy,MaxRestart,MaxTime}) ->
    {ok,{{RestartStrategy,MaxRestart,MaxTime},
         [
          {worker1_id,                          %一个id标识
           {worker1, start_link, []},           %此行，批示一个fun  : {Mod ,fun, param}
           permanent, 1000, worker, [worker1]}, %死后必须重重 A permanent process should always be restarted, no matter what.

          %% 1000的含义是，首先给子进程1000ms 的时间进行关闭，1000ms后，若子进程还未正常关闭，强关。即先礼后兵
          %% 或不是子进程还是一个子树，则一般设为infinity，以便给予足够关闭时间 。 %% 也可取值，brutal_kill，就地斩于马下。
          %% 但是对于simple_one_for_one 的场合似乎不守此规则，，似乎对子进程有点放任自流，自生自灭的
          %% 感觉the supervisor will just exit and it will be left to each of the workers to
          %% terminate on their own, after their supervisor is gone.

          %% [worker1] 的 是子模块名，始终只有一个元素，但是如果子模块是gen_event 类型的，必须设成dynamic,而不是子模块名。

          {worker2_id,
           {worker2, start_link, []},
           %%transient 介于permanent 与temporary之间，此进程若正常die(normal) 则不重启，否则 重启。使命已完成，无重生必要
           %% 适用于一次性完成任务后，就不再启用的场合。
           transient, 1000, worker, [worker2]},
          {worker3_id,
           {worker3, start_link, []},
           temporary, 1000, worker, [worker3]} %死了就死了不必重启。a temporary process is a process that should never be restarted.
         ]
        }}.

%% 关于 -> one_for_one 与 permanent transient temporary 的组合。
%% one_for_one 当一个worker die,只重启此死worker
%% one_for_all 当一个worker die ,重启所有的worker 及其子supervisor

%% permanent %死后必须重重 A permanent process should always be restarted, no matter what.
%%transient 介于permanent 与temporary之间，此进程若正常die(normal) 则不重启，否则 重启。使命已完成，无重生必要
%%temporary 贱命一条，死既死尔，不必重启。a temporary process is a process that should never be restarted.

%% test
%% sup:start1().
%% worker1:shutdown().
%% worker2:shutdown().
%% worker3:shutdown().
%% worker1:stop().
%% worker2:stop().
%% worker3:stop().

%% sup:start3().
%% worker1:shutdown().
%% worker2:shutdown().
%% worker3:shutdown().
%% worker1:stop().
%% worker2:stop().
%% worker3:stop().

%% sup:start2().
%% worker1:shutdown().
%% worker2:shutdown().
%% worker3:shutdown().
%% worker1:stop().
%% worker2:stop().
%% worker3:stop().

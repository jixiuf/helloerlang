-module(spawn_test).
%% http://www.lshift.net/blog/2006/09/10/how-fast-can-erlang-create-processes
%% 测试 erlang创建进程的速度
 %% spawntest:serial_spawn(1).
%% 3.58599e+5
%% 创建1000000个进程消耗的时间
-export([serial_spawn/1]).

serial_spawn(M) ->
    N = 1000000,
    NpM = N div M,
    Start = erlang:now(),
    dotimes(M, fun () -> serial_spawn(self(), NpM) end),
    dotimes(M, fun () -> receive X -> X end end),
    Stop = erlang:now(),
    (NpM * M) / time_diff(Start, Stop).

serial_spawn(Who, 0) -> Who ! done;
serial_spawn(Who, Count) ->
    spawn(fun () ->
          serial_spawn(Who, Count - 1)
      end).

dotimes(0, _) -> done;
dotimes(N, F) ->
    F(),
    dotimes(N - 1, F).

time_diff({A1,A2,A3}, {B1,B2,B3}) ->
    (B1 - A1) * 1000000 + (B2 - A2) + (B3 - A3) / 1000000.0 .

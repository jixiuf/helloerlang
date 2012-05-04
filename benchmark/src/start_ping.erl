-module(start_ping).
-export([start/2]).

% start ping
start(PongNode, Num) ->
    register(flood_ping, spawn(fun() -> ping_loop(Num, now(), Num) end)),
    send(PongNode, Num).

send(_PongNode, 0) ->
    ok;
send(PongNode, Num) ->
    % send a spawned ping
    spawn(fun() -> {flood_pong, PongNode} ! {{flood_ping, node()}, ping_request} end),
    send(PongNode, Num - 1).

ping_loop(Num, Start, 0) ->
    T = timer:now_diff(now(), Start),
    io:format("RECEIVED ALL ~p in ~p ms [~p/min]~n",[Num, T, (Num*60000000/T)]);
ping_loop(Num, Start, Count) ->
    receive
        {pong, _PingBack} ->
            ping_loop(Num, Start, Count-1);
        _Received ->
            ping_loop(Num, Start, Count)
    after 10000 ->
        io:format("ping timeout, missing ~p pong, shutdown~n",[Count])
    end.

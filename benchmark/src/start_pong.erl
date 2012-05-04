-module(start_pong).
-export([start/0]).

% starts pong
start() ->
    register(flood_pong, spawn(fun() -> pong_loop() end)).

pong_loop() ->
    receive
        {{Sender, SenderNode}, Any} ->
            % pong back
            {Sender, SenderNode} ! {pong, Any},
            pong_loop();
        shutdown ->
            io:format("pong shutdown~n",[]);
        _Ignore ->
            pong_loop()
    after 60000 ->
        io:format("pong timeout, shutdown~n",[])
    end.

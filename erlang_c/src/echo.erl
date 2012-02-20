%%echo.erl
%% echo.erl
-module(echo).
-export([start/0, stop/0, echo/1]).

start() ->
    spawn(fun() ->
                  register(echo, self()),
                  process_flag(trap_exit, true),
                  %% 这里指定packet 为1，前数据格式为 <<Len:8,Data>>
                  Port = open_port({spawn, "echo.exe"}, [{packet, 1}]),
                  %% Port = open_port({spawn, "./echo"}, [{packet, 1}]),
                  loop(Port)
          end).

stop() ->
    echo ! stop.

echo(Msg) ->
    echo ! {call, self(), Msg},    %% Msg必须是一个List
    receive
        Result -> Result
    after 1000 -> io:format("time out~n"), true
    end.

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            %% （三）、向端口进程发送消息。
            %%     Port ! {PidC, {command, Data}}
            %% PidC 为 Port 端口对应的processId
            Port ! {self(), {command, Msg}},    %% Msg必须是一个List
            receive
                {Port, {data, Data}} ->     %% 返回的Data也是一个List
                    Caller ! Data
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} -> exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            io:format("port terminated!~n"),
            exit({port_terminated, Reason})
    end.

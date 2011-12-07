-module(worker3).
-behaviour(gen_server).
-compile([export_all]).

start_link()->
    {ok,Pid}= gen_server:start_link(?MODULE,[],[]),
    register(worker3,Pid),
    {ok,Pid}
        .
stop()->
    gen_server:cast(whereis(worker3),stop)
        .

shutdown()->
    gen_server:cast(whereis(worker3),shutdown)
        .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([])->
    io:format("hello ,I am worker 3 ,I am working now...~n",[]),
    {ok,[]}
    .

handle_cast(stop,State)->
    io:format("worker3 stoping...~n",[]),
    {stop,normal ,State};
handle_cast(shutdown,State)->
    io:format("worker3 shutdowning unnormal...~n",[]),
    {stop,shutdown_reason ,State}
        .

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(Reason,_State)->
    io:format("worker3 terminaled with reason: ~p ~n",[Reason]),
    ok.

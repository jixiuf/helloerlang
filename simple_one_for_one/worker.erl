-module(worker).
-export([start_link/1,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([stop/1]).

start_link(S)->
    gen_server:start_link(?MODULE,S,[])
.
init(S)->
    io:format("~p~n",[S]),
    self()! ("hello"++S),
    {ok,S}
        .

handle_call(stop,_From,State)->
    {stop,State};
handle_call(_Request,_From,State)->
    {noreply, State}
        .

handle_cast(_Request,State)->
    {noreply, State} .

handle_info(Msg,State) ->
    io:format("got msg:~s~n",[Msg]),
    {stop,normal,State}.
    %% {noreply, State}.

terminate(_Reason,_State)->
    io:format("a worker die!~n",[]),
    ok .

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .

%% interface
stop(Pid)->
    gen_server:call(Pid,stop).

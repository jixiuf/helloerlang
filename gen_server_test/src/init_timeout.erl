-module(init_timeout).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-export([start_link/0]).

start_link()->
    gen_server:start_link(?MODULE,[],[])
        .

init(S)->
    %% 3s
    Timeout=3000,
    {ok, S,Timeout}
        .

handle_call(_Request,_From,State)->
    {noreply, State}
        .

handle_cast(_Request,State)->
    {noreply, State} .

handle_info(timeout,State)->
    io:format("init timeout...~n",[]),
    {noreply, State}
    ;
handle_info(_Info,State)->
    io:format("handle_info/2 is called~n",[]) ,
    {noreply, State}.

terminate(_Reason,_State)->
    ok .

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .

%% {ok,P}=init_timeout:start_link().
%% 3s后会出现超时
%% gen_server:call(P,hello).

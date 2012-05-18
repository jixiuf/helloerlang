-module(hello_gen).
-vsn("0.1").

-export([start_link/0,hello/0]).

-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

hello()->
    gen_server:call(?MODULE,hello)
    .
start_link()->
    gen_server:start_link({local,?MODULE},?MODULE,[],[])
        .

init(_S)->
    %%最初始的state 是 1
    io:format("hello_gen is started~n",[]) ,
    {ok, 1}
        .


handle_call(hello,_From,State)->
    {reply,State, State};
handle_call(_Request,_From,State)->
    {noreply, State}
        .

handle_cast(_Request,State)->
    {noreply, State} .

handle_info(_Info,State)->
    {noreply, State}.

terminate(_Reason,_State)->
    ok .

code_change("0.2",State,Extra)->
    %% 从0.2 隆为当前0.1
    io:format("downgrade from 0.2 to 0.1~n",[]) ,
    io:format("and the third param Extra is ~p~n",[Extra]),
    {state,Num}=State,
    {ok,Num};
code_change(_Previous_Version,State,_Extra)->
    {ok,State} .

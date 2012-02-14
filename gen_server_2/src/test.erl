-module(test).
-export([terminate/2,handle_cast/2,handle_call/3,init/1,start_link/0]).



start_link()->
    gen_server_2:start_link(?MODULE,[])
        .

init(State)->
    {ok,State}
        .

handle_call(hello,_From,State)->
    io:format("you say hello~n",[]),
    {reply,you_say_hello, State};
handle_call(world,From,State)->
    Reply=gen_server_2:reply(self(),you_say_world),
    io:format("~p:~p~n",[self(),From]),
    io:format("the reply is :~p~n",[Reply]) ,
    {noreply, State};
handle_call(terminate,_From,State)->
    {stop, shutdown, shutdown, State};
handle_call(_Req,_From,State)->
    {noreply, State}
        .

handle_cast(hello,State)->
    io:format("you say hello by cast!~n",[]),
    {noreply, State} ;
handle_cast(world,State)->
    io:format("you say world by cast!~n",[]),
    {noreply, State} .


terminate(_Reason,_State)->
    ok .

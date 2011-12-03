-module(msg_client).
-compile([export_all]).
-include("msg_config.hrl").

message(To,Msg)->
    case whereis(client) of
        undefined->
            you_are_not_logged_on;
        _ ->
            client! #message_req{to_name=To,msg=Msg}
    end
    %% {server,server@jf.org} !{message2,[To,self(),Msg]}
    %% receive
    %%     {message2,{From,Msg}}->
    %%         io:format(" ~p: ~p\n",[To,Msg])
    %% end
        .
receive_msg()->
    receive
        #message_send{from_pid=From,msg=Msg} ->
            io:format(" ~p: ~p\n",[From,Msg]);
        #message_req{to_name=To,msg=Msg} ->
            {server,?server_node} ! #message2{to_name=To,from_pid=self(),msg=Msg};
        accept->
            io:format("acceped~n",[]),
            true;
        reject->
            io:format("rej~n",[]),
            false
    end,
    receive_msg()
        .
logon(Username)->
    case whereis(client) of
        undefined ->
            register(client, spawn(?MODULE,receive_msg,[])),
            {server,?server_node} ! #logon{username=Username,from_pid=whereis(client)} ;
        _ ->
            already_logged_on
    end.
%% erl -name server
%% msg:start_server().
%% erl -name c1
%%  msg:logon("c1").
%%  erl -name c2
%%  msg:logon("c2").
%%  msg:message("c1","hello,c1,I am c2").
%%  msg:message("c2","hello,c2,I am c1").


%% whereis(RegName) ->pid or port or undefined.

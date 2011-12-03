-module(msg_server).
-compile([export_all]).
-include("msg_config.hrl").

start_server()->
    register (server,spawn(?MODULE,server,[[]])).
server(UserList)->
    receive
        #logon{username=UserName,from_pid=FromPid} ->
            server(handle_logon(UserName,FromPid,UserList));
        #message2{to_name=To,from_pid=From,msg=Msg}->
            io:format("a conn~n",[]),
            io:format("~w~n",[UserList]),
            case lists:keysearch(To,1,UserList) of
                false ->
                    io:format("user ~p doesnt exists ~n", [To]);
                {value,{_,UserPid}}->
                    io:format("Msg to ~p ~p ~n", [To,UserPid]),
                    UserPid! #message_send{from_pid=From,msg=Msg}
            end,
            server(UserList);
        _ ->
            server(UserList)
    end.
handle_logon(Username,FromPid,UserList)->
    case lists:keymember(Username,1,UserList) of
        true ->
            FromPid ! reject,
            io:format("server rejected ~p\n",[Username]),
            UserList;
        false ->
            FromPid!accept,
            io:format("server acceped ~p\n",[Username]),
            [{Username,FromPid}|UserList]
    end
        .

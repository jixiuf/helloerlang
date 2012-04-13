-module(emysql_execute).
-export([execute/1]).
-include("tbl_fileds.hrl").
-include_lib("../deps/emysql/include/emysql.hrl").


-import(emysql_center, [quote/1]).
execute({addOnOff,#tbl_onoff{}=Record}) ->
    try
        Sql="call addOnOff(" ++
            quote(Record#tbl_onoff.onOffType            ) ++","++
            quote(Record#tbl_onoff.accountId            ) ++","++
            quote(Record#tbl_onoff.accountType          ) ++","++
            quote(Record#tbl_onoff.playerId             ) ++","++
            quote(Record#tbl_onoff.playerLevel          ) ++","++
            quote(Record#tbl_onoff.playerName           ) ++","++
            quote(Record#tbl_onoff.datetime             ) ++","++
            quote(Record#tbl_onoff.clientVersion        ) ++","++
            quote(Record#tbl_onoff.clientType           ) ++","++
            quote(Record#tbl_onoff.issuers              ) ++","++
            quote(Record#tbl_onoff.flashPlayerVersion   ) ++","++
            quote(Record#tbl_onoff.connectType          ) ++","++
            quote(Record#tbl_onoff.gameServerName       ) ++
            ")",
        io:format("~p~n",[Sql]) ,
        Result=emysql:execute(hello_pool, list_to_binary(Sql)),
        case Result of
            #error_packet{}->
                error;
            #ok_packet{}->
                ok
        end

    catch
        error:_Error->
            ok
    end;
execute(V) ->
    io:format("~n~p",[V]).

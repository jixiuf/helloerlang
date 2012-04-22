-module(emysql_execute).
-export([execute/1]).
-include("tbl_fileds.hrl").
-include("game_log_server_macro.hrl").
-include_lib("emysql/include/emysql.hrl").

-import(emysql_center, [quote/1]).
execute({insert_tbl_reg,#tbl_reg{}=Record}) ->
    try
        Sql="call insert_tbl_reg(" ++
            quote(Record#tbl_reg.accountType                               ) ++","++
            quote(Record#tbl_reg.playerId                                  ) ++","++
            quote(Record#tbl_reg.playerName                                ) ++","++
            quote(Record#tbl_reg.logTime                                  ) ++","++
            quote(Record#tbl_reg.clientVersion                             ) ++","++
            quote(Record#tbl_reg.clientType                                ) ++","++
            quote(Record#tbl_reg.issuers                                   ) ++","++
            quote(Record#tbl_reg.flashPlayerVersion                        ) ++","++
            quote(Record#tbl_reg.connectType                               ) ++","++
            quote(Record#tbl_reg.gameServerName                            ) ++","++
            quote(Record#tbl_reg.ipAddr                                    ) ++","++
            quote(Record#tbl_reg.accountId                                 ) ++
            ")",
        io:format("sql:~p~n",[Sql]) ,
        Result=emysql:execute(?DEF_POOL_ID, list_to_binary(Sql)),
        case Result of
            #error_packet{}=R->
                io:format("~p~n",[R]) ,
                error;
            #ok_packet{}->
                ok
        end

    catch
        error:Error->
            io:format("error:~p~n",[Error]),
            error
    end;
execute({insert_tbl_onlinenum,#tbl_onlinenum{}=Record}) ->
    try
        Sql="call insert_tbl_onlinenum(" ++
            quote(Record#tbl_onlinenum.onlinenum                            ) ++","++
            quote(Record#tbl_onlinenum.gameServerName                       ) ++","++
            quote(Record#tbl_onlinenum.logTime                             ) ++
            ")",
        io:format("sql:~p~n",[Sql]) ,
        Result=emysql:execute(?DEF_POOL_ID, list_to_binary(Sql)),
        case Result of
            #error_packet{}=R->
                io:format("~p~n",[R]) ,
                error;
            #ok_packet{}->
                ok
        end

    catch
        error:Error->
            io:format("error:~p~n",[Error]),
            error
    end;
execute({insert_tbl_item_change,#tbl_item_change{}=Record}) ->
    try
        Sql="call insert_tbl_item_change(" ++
            quote(Record#tbl_item_change.accountType                    ) ++","++
            quote(Record#tbl_item_change.playerId                       ) ++","++
            quote(Record#tbl_item_change.playerName                     ) ++","++
            quote(Record#tbl_item_change.itemId                         ) ++","++
            quote(Record#tbl_item_change.itemTid                        ) ++","++
            quote(Record#tbl_item_change.itemPosId                      ) ++","++
            quote(Record#tbl_item_change.extList                        ) ++","++
            quote(Record#tbl_item_change.oT_OLDEQUIP_ID                 ) ++","++
            quote(Record#tbl_item_change.oT_OLDEQUIP_SOURCE_DATA_INDEX  ) ++","++
            quote(Record#tbl_item_change.oT_ACTION_TYPE                 ) ++","++
            quote(Record#tbl_item_change.oT_ACTION_TIME                 ) ++","++
            quote(Record#tbl_item_change.oT_EQUIPMENT_MAP               ) ++","++
            quote(Record#tbl_item_change.oT_EQUIPMENT_NOTE              ) ++","++
            quote(Record#tbl_item_change.oT_ISSUERS_ID                  ) ++","++
            quote(Record#tbl_item_change.oT_GAME_SERVER_NAME            ) ++","++
            quote(Record#tbl_item_change.oT_GAME_ZONE_NAME              ) ++","++
            quote(Record#tbl_item_change.account                        ) ++
            ")",
        io:format("sql:~p~n",[Sql]) ,
        Result=emysql:execute(?DEF_POOL_ID, list_to_binary(Sql)),
        case Result of
            #error_packet{}=R->
                io:format("~p~n",[R]) ,
                error;
            #ok_packet{}->
                ok
        end

    catch
        error:Error->
            io:format("error:~p~n",[Error]),
            error
    end;
execute({insert_tbl_consume_item,#tbl_consume_item{}=Record}) ->
    try
        Sql="call insert_tbl_consume_item(" ++
            quote(Record#tbl_consume_item.accountType            ) ++","++
            quote(Record#tbl_consume_item.playerId               ) ++","++
            quote(Record#tbl_consume_item.playerName             ) ++","++
            quote(Record#tbl_consume_item.consumeItem            ) ++","++
            quote(Record#tbl_consume_item.consumeCount           ) ++","++
            quote(Record#tbl_consume_item.itemType               ) ++","++
            quote(Record#tbl_consume_item.itemTid                ) ++","++
            quote(Record#tbl_consume_item.itemId                 ) ++","++
            quote(Record#tbl_consume_item.itemSellCoin           ) ++","++
            quote(Record#tbl_consume_item.logTime               ) ++","++
            quote(Record#tbl_consume_item.mapId                  ) ++","++
            quote(Record#tbl_consume_item.sellNpcId              ) ++","++
            quote(Record#tbl_consume_item.clientVersion          ) ++","++
            quote(Record#tbl_consume_item.clientType             ) ++","++
            quote(Record#tbl_consume_item.issuers                ) ++","++
            quote(Record#tbl_consume_item.flashPlayerVersion     ) ++","++
            quote(Record#tbl_consume_item.connectType            ) ++","++
            quote(Record#tbl_consume_item.gameServerName         ) ++","++
            quote(Record#tbl_consume_item.ipAddr                 ) ++","++
            quote(Record#tbl_consume_item.accountId            ) ++
            ")",
        io:format("sql:~p~n",[Sql]) ,
        Result=emysql:execute(?DEF_POOL_ID, list_to_binary(Sql)),
        case Result of
            #error_packet{}=R->
                io:format("~p~n",[R]) ,
                error;
            #ok_packet{}->
                ok
        end

    catch
        error:Error->
            io:format("error:~p~n",[Error]),
            error
    end;
execute({insert_tbl_add_item,#tbl_add_item{}=Record}) ->
    try
        Sql="call insert_tbl_add_item(" ++
            quote(Record#tbl_add_item.accountType          ) ++","++
            quote(Record#tbl_add_item.playerId             ) ++","++
            quote(Record#tbl_add_item.playerName           ) ++","++
            quote(Record#tbl_add_item.addItemType          ) ++","++
            quote(Record#tbl_add_item.itemCount            ) ++","++
            quote(Record#tbl_add_item.itemType             ) ++","++
            quote(Record#tbl_add_item.itemTid              ) ++","++
            quote(Record#tbl_add_item.itemId               ) ++","++
            quote(Record#tbl_add_item.needCoin             ) ++","++
            quote(Record#tbl_add_item.needMoney            ) ++","++
            quote(Record#tbl_add_item.logTime             ) ++","++
            quote(Record#tbl_add_item.mapId                ) ++","++
            quote(Record#tbl_add_item.giverId              ) ++","++
            quote(Record#tbl_add_item.taskId               ) ++","++
            quote(Record#tbl_add_item.addItemResult        ) ++","++
            quote(Record#tbl_add_item.clientVersion        ) ++","++
            quote(Record#tbl_add_item.clientType           ) ++","++
            quote(Record#tbl_add_item.issuers              ) ++","++
            quote(Record#tbl_add_item.flashPlayerVersion   ) ++","++
            quote(Record#tbl_add_item.connectType          ) ++","++
            quote(Record#tbl_add_item.gameServerName       ) ++","++
            quote(Record#tbl_add_item.ipAddr               ) ++","++
            quote(Record#tbl_add_item.accountId            ) ++
            ")",
        io:format("sql:~p~n",[Sql]) ,
        Result=emysql:execute(?DEF_POOL_ID, list_to_binary(Sql)),
        case Result of
            #error_packet{}=R->
                io:format("~p~n",[R]) ,
                error;
            #ok_packet{}->
                ok
        end

    catch
        error:Error->
            io:format("error:~p~n",[Error]),
            error
    end;
execute({insert_tbl_add_gold,#tbl_add_gold{}=Record}) ->
    try
        Sql="call insert_tbl_add_gold(" ++
            quote(Record#tbl_add_gold.accountType         ) ++","++
            quote(Record#tbl_add_gold.playerId            ) ++","++
            quote(Record#tbl_add_gold.playerName          ) ++","++
            quote(Record#tbl_add_gold.addGoldType         ) ++","++
            quote(Record#tbl_add_gold.addMoney            ) ++","++
            quote(Record#tbl_add_gold.addCoin             ) ++","++
            quote(Record#tbl_add_gold.giverId             ) ++","++
            quote(Record#tbl_add_gold.taskId              ) ++","++
            quote(Record#tbl_add_gold.logTime            ) ++","++
            quote(Record#tbl_add_gold.mapId               ) ++","++
            quote(Record#tbl_add_gold.sellItemType        ) ++","++
            quote(Record#tbl_add_gold.sellItemTid         ) ++","++
            quote(Record#tbl_add_gold.sellItemId          ) ++","++
            quote(Record#tbl_add_gold.clientVersion       ) ++","++
            quote(Record#tbl_add_gold.clientType          ) ++","++
            quote(Record#tbl_add_gold.issuers             ) ++","++
            quote(Record#tbl_add_gold.flashPlayerVersion  ) ++","++
            quote(Record#tbl_add_gold.gameServerName      ) ++","++
            quote(Record#tbl_add_gold.ipAddr              ) ++","++
            quote(Record#tbl_add_gold.accountId           ) ++
            ")",
        io:format("sql:~p~n",[Sql]) ,
        Result=emysql:execute(?DEF_POOL_ID, list_to_binary(Sql)),
        case Result of
            #error_packet{}=R->
                io:format("~p~n",[R]) ,
                error;
            #ok_packet{}->
                ok
        end

    catch
        error:Error->
            io:format("error:~p~n",[Error]),
            error
    end;
execute({insert_tbl_add_exp,#tbl_add_exp{}=Record}) ->
    try
        Sql="call insert_tbl_add_exp(" ++
            quote(Record#tbl_add_exp.accountType             ) ++","++
            quote(Record#tbl_add_exp.playerId                ) ++","++
            quote(Record#tbl_add_exp.playerName              ) ++","++
            quote(Record#tbl_add_exp.addExpType              ) ++","++
            quote(Record#tbl_add_exp.isLevelUp               ) ++","++
            quote(Record#tbl_add_exp.oldLevel                ) ++","++
            quote(Record#tbl_add_exp.newLevel                ) ++","++
            quote(Record#tbl_add_exp.addExp                  ) ++","++
            quote(Record#tbl_add_exp.giverId                 ) ++","++
            quote(Record#tbl_add_exp.taskId                  ) ++","++
            quote(Record#tbl_add_exp.logTime                ) ++","++
            quote(Record#tbl_add_exp.mapId                   ) ++","++
            quote(Record#tbl_add_exp.clientVersion           ) ++","++
            quote(Record#tbl_add_exp.clientType              ) ++","++
            quote(Record#tbl_add_exp.issuers                 ) ++","++
            quote(Record#tbl_add_exp.flashPlayerVersion      ) ++","++
            quote(Record#tbl_add_exp.connectType             ) ++","++
            quote(Record#tbl_add_exp.gameServerName          ) ++","++
            quote(Record#tbl_add_exp.ipAddr                  ) ++","++
            quote(Record#tbl_add_exp.accountId               ) ++
            ")",
        io:format("sql:~p~n",[Sql]) ,
        Result=emysql:execute(?DEF_POOL_ID, list_to_binary(Sql)),
        case Result of
            #error_packet{}=R->
                io:format("~p~n",[R]) ,
                error;
            #ok_packet{}->
                ok
        end

    catch
        error:Error->
            io:format("error:~p~n",[Error]),
            error
    end;
execute({insert_tbl_on_off,#tbl_on_off{}=Record}) ->
    try
        Sql="call insert_tbl_on_off(" ++
            quote(Record#tbl_on_off.accountId            ) ++","++
            quote(Record#tbl_on_off.accountType          ) ++","++
            quote(Record#tbl_on_off.playerId             ) ++","++
            quote(Record#tbl_on_off.headHeroId           ) ++","++
            quote(Record#tbl_on_off.playerLevel          ) ++","++
            quote(Record#tbl_on_off.playerName           ) ++","++
            quote(Record#tbl_on_off.logTime             ) ++","++
            quote(Record#tbl_on_off.clientVersion        ) ++","++
            quote(Record#tbl_on_off.clientType           ) ++","++
            quote(Record#tbl_on_off.issuers              ) ++","++
            quote(Record#tbl_on_off.flashPlayerVersion   ) ++","++
            quote(Record#tbl_on_off.connectType          ) ++","++
            quote(Record#tbl_on_off.gameServerName       ) ++","++
            quote(Record#tbl_on_off.ipAddr               ) ++","++
            quote(Record#tbl_on_off.onOffType            ) ++
            ")",
        io:format("sql:~p~n",[Sql]) ,
        Result=emysql:execute(?DEF_POOL_ID, list_to_binary(Sql)),
        case Result of
            #error_packet{}=R->
                io:format("~p~n",[R]) ,
                error;
            #ok_packet{}->
                ok
        end

    catch
        error:Error->
            io:format("error:~p~n",[Error]),
            error
    end.

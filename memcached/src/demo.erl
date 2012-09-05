-module(demo).
-export([start/0,stop/0]).

-define(MEM_KEY_PREFIX,"0.").
%% -record(playerGenderIndex,{index,pid,level,pairedFriendTime=0,country="none",province="none",city="none"}). %%
-record(gameData,{key,value}).

start()->
    emc:start_link().
stop()->
    ok.

create_db()->
    ok.

delete_db()->
    emc:flush_all().
upgrade_db()->
    ok.
reset_db()->
    delete_db(),
    create_db()    .
clearTable(_Tab)->false.

load_game_data(Key)->
    case getValue("gameData."++getDBVersion()++emc:to_list(Key),none) of
        none->none;
        #gameData{key=_Key,value=Value} ->
            Value
        end
    .
save_game_data(GameData)->
    #gameData{key=Key,value=_Value}=GameData,
    setValue("gameData."++getDBVersion()++emc:to_list(Key),GameData)
    .


getDBVersion()->
    ?MEM_KEY_PREFIX
    .

getValue(Key,Default)when is_integer(Default)->
    case emc:get_value(Key) of
        {values,[{value,Key,_Flag,Value}]}->
            if
                byte_size(Value) =:=0-> Default;
                true -> emc:to_integer(Value)
            end;
        _->
            Default
    end;
getValue(Key,Default)->
    case emc:get_value(Key) of
        {values,[{value,Key,_Flag,Value}]}->
            if
                byte_size(Value) =:=0-> Default;
                true -> binary_to_term(Value)
            end;
        _->
            Default
    end.

getValueCas(Key,Default)->
    case emc:get_value(Key) of
        {values,[{valueWithCas,Key,_Flag,CasUnique,Value}]}->
            {binary_to_term(Value),CasUnique};
        _->
            Default
    end.

setValue(Key,Value)->
    case emc:set_value(Key,Value) of
        {ok,stored}->true;
        _ ->false
    end.
addValue(Key,Value)->
    case emc:add_value(Key,Value) of
        {ok,stored}->true;
        _ ->false
    end.

delValue(Key)->
    case emc:delete_value(Key) of
        {ok,deleted}->true;
        _ ->false
    end.
incValue(Key,Offset,Default)->
    case emc:inc_value(Key,Offset) of
        {ok,Value}->
            emc:to_integer(Value);
        {not_found}->
            case emc:add_value(Key,Default) of
                {not_stored}->
                    incValue(Key,Offset,Default);
                {ok,stored} ->1
            end;
        {error} ->
            false;
        Value ->
            io:format("incValue ~p~p~n",[Key,Value])
    end

        .
getNewId(Key)->
    MemKey="couter."++getDBVersion()++emc:to_list(Key),
    incValue(MemKey,1,1)
    .

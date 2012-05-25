-module(record_util).
%%%-------------------------------------------------------------------
%%% File        : record_util.erl
%%% Author      : 纪秀峰 jixiuf@gmail.com
%%% Description : utilities for manipulating records
%%%
%%% Created     :  2 Sep 2008 by Gordon Guthrie
%%%-------------------------------------------------------------------
%% http://trapexit.org/Match%5FSpecifications%5FAnd%5FRecords%5F%28Dynamically!%29
%% http://forum.trapexit.org/viewtopic.php?p=21790
%% -include("myhead.hrl").
-define(HEAD_FILE_PATH,"../include/myhead.hrl").%relative to ebin/
%% save ?MODULENAME.erl in this dir
-define(DEST_DIR,"../src").                     %relative to ebin/
-define(MODULENAME,"myhead_util").
-define(INCLUDE_CMD_IN_DEST_MODULE,"-include(\"myhead.hrl\").").

-export([make/0]).


make() ->
    {ok,Tree}=epp:parse_file(?HEAD_FILE_PATH,["./"],[]),
    Src=make_src(Tree),
    ok=file:write_file(filename:join([?DEST_DIR,?MODULENAME])++".erl",list_to_binary(Src)).

make_src(Tree) -> make_src(Tree,[]).

make_src([],Acc)                              ->
    top_and_tail([make_index(Acc,[]),
                  "\n",
                  make_value(Acc,[]),
                  "\n",
                  make_key(Acc,[]),
                  "\n",
                  make_info(Acc,[]),
                  "\n",
                  make_length(Acc,[])]);
make_src([{attribute,_,record,Record}|T],Acc) -> make_src(T,[Record|Acc]);
make_src([_H|T],Acc)                          -> make_src(T,Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_index([],Acc1)    ->
    Head="%% get the filed index (1 based ) of a record\n",
    Head++
    lists:flatten(Acc1)++
        "get_index(Record,_Field) -> exit({error,\""++
        "Invalid Record Name: \"++Record}).\n";
make_index([{RecName,Def}|T],Acc1) ->
    NewAcc1=expand_index(RecName,Def,1,[]),
    make_index(T,[NewAcc1|Acc1]).

expand_index(Name,[],_N,Acc) -> lists:reverse([mk_get_index(Name)|Acc]);
expand_index(Name,[{record_field,_,{atom,_,F},_}|T],N,Acc) ->
    expand_index(Name,T,N+1,[mk_get_index(Name,F,N)|Acc]);
expand_index(Name,[{record_field,_,{atom,_,F}}|T],N,Acc) ->
    expand_index(Name,T,N+1,[mk_get_index(Name,F,N)|Acc]);
expand_index(Name,[_H|T],N,Acc) -> expand_index(Name,T,N+1,Acc).

%% mk_get_index/1 builds an error line
mk_get_index(Name) -> "get_index("++atom_to_list(Name)++",F) -> "++
        "exit({error,\"Record: "++atom_to_list(Name)++
        " has no field called \"++atom_to_list(F)});\n".

mk_get_index(Name,Field,_N) ->
    "get_index("++atom_to_list(Name)++","++
    atom_to_list(Field)++")-> #"++atom_to_list(Name)++"."++atom_to_list(Field)++"-1 ;\n"++
        "get_index(Record,"++atom_to_list(Field)++") when is_record(Record,"++atom_to_list(Name)++")-> #"++
        atom_to_list(Name)++"."++atom_to_list(Field)++"-1 ;\n"
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_key([],Acc1)    ->
    Head="%% get field name by index\n",
    Head++
    lists:flatten(Acc1)++
        "get_key(Record,_Index) -> exit({error,\""++
        "Invalid  Record Name or Index out of bound : \"++lists:flatten(io_lib:format(\"~p\",[Record]))}).\n";
make_key([{RecName,Def}|T],Acc1) -> NewAcc1=expand_key(RecName,Def,1,[]),
                  make_key(T,[NewAcc1|Acc1]).


expand_key(Name,[],_N,Acc) -> lists:reverse([mk_get_key(Name)|Acc]);
expand_key(Name,[{record_field,_,{atom,_,F},_}|T],N,Acc) ->
    expand_key(Name,T,N+1,[mk_get_key(Name,F,N)|Acc]);
expand_key(Name,[{record_field,_,{atom,_,F}}|T],N,Acc) ->
    expand_key(Name,T,N+1,[mk_get_key(Name,F,N)|Acc]);
expand_key(Name,[_H|T],N,Acc) -> expand_key(Name,T,N+1,Acc).

%% mk_get_key/1 builds an error line
mk_get_key(Name) -> "get_key("++atom_to_list(Name)++",Index) -> "++
        "exit({error,\"Record: "++atom_to_list(Name)++
        " has no field index \"++integer_to_list(Index)});\n".

mk_get_key(Name,Field,N) ->
    "get_key("++atom_to_list(Name)++","++
    integer_to_list(N)++")-> "++atom_to_list(Field)++";\n"++
    "get_key(Record,"++integer_to_list(N)++") when is_record(Record,"++atom_to_list(Name)++") ->"++
     atom_to_list(Field)++";\n"
        .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_length([],Acc1)->
    Head="%% get count of fields of a record\n",
    Tail1="length(Other) -> exit({error,\"Invalid Record Name: \""++
    "++Other}).\n",
    [Head|lists:reverse([Tail1|Acc1])];
make_length([{RecName,Def}|T],Acc1)->
    Cause= "length("++atom_to_list(RecName)++") -> "++
           integer_to_list(length(Def))++";\n"++
        "length(Record ) when is_record(Record,"++atom_to_list(RecName)++")->"++
           integer_to_list(length(Def))++";\n",
    make_length(T,[Cause|Acc1])
    .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_info([],Acc1)->
    Head="%% get all field name of a record\n",
    Tail1="fields_info(Other) -> exit({error,\"Invalid Record Name: \""++
    "++Other}).\n",
    [Head|lists:reverse([Tail1|Acc1])];
make_info([{RecName,Def}|T],Acc1)->
    Fields=[F|| {record_field,_Num,{atom,_Num2,F}} <- Def ],
    Cause= "fields_info("++atom_to_list(RecName)++") -> "++
           io_lib:format("~p",[Fields])++";\n"++
        "fields_info(Record ) when is_record(Record,"++atom_to_list(RecName)++")->"++
           io_lib:format("~p",[Fields])++";\n",
    make_info(T,[Cause|Acc1])
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_value([],Acc1)    ->
    Head="%% get field value of a record by index(1 based) or field name\n",
    Tail=       "get_value(Record,_KeyIndex) -> exit({error,\""++
        "Invalid Record Name: \"++Record}).\n",
    Head++ lists:flatten(Acc1) ++ Tail;
make_value([{RecName,Def}|T],Acc1) -> NewAcc1=expand_value(RecName,Def,1,[]),
                  make_value(T,[NewAcc1|Acc1]).


expand_value(Name,[],_N,Acc) -> lists:reverse([mk_get_value(Name)|Acc]);
expand_value(Name,[{record_field,_,{atom,_,F},_}|T],N,Acc) ->
    expand_value(Name,T,N+1,[mk_get_value(Name,F,N)|Acc]);
expand_value(Name,[{record_field,_,{atom,_,F}}|T],N,Acc) ->
    expand_value(Name,T,N+1,[mk_get_value(Name,F,N)|Acc]);
expand_value(Name,[_H|T],N,Acc) -> expand_value(Name,T,N+1,Acc).

%% mk_get_value/1 builds an error line
mk_get_value(Name) -> "get_value("++atom_to_list(Name)++",Index) -> "++
        "exit({error,\"Record: "++atom_to_list(Name)++
        " has no field index \"++integer_to_list(Index)});\n".

mk_get_value(Name,Field,N) ->
    "get_value(Record,"++atom_to_list(Field)++") when  is_record(Record,"++atom_to_list(Name)++")->"++
     "Record#"++atom_to_list(Name)++"."++atom_to_list(Field)++";\n"++
    "get_value(Record,"++integer_to_list(N)++") when  is_record(Record,"++atom_to_list(Name)++")->"++
     "Record#"++atom_to_list(Name)++"."++atom_to_list(Field)++";\n"
        .

top_and_tail(Acc1)->
    Top="%% This module automatically generated - do not edit\n"++
    "\n"++
    "%%% This module provides utilities for getting info about records\n"++
    "%% suppose there is a record in myhead.erl\n\n"++
    "%% -record(user,[id,name,age]).\n"++
    "%% U=#user{id=100,name=joseph,age=11}.\n\n"++
    "%% get_index(user,id)==1\n"++
    "%% get_index(U,id)==1\n\n"++
    "%% get_value(U,id)==100\n"++
    "%% get_value(U,1)==100\n\n"++
    "%% get_key(U,1)==key\n"++
    "%% get_key(user,1)==key\n\n"++
    "%% fields_info(user)==[id,name,age]\n"++
    "%% fields_info(U)==[id,name,age]\n\n"++
    "%% length(user)==3\n"++
    "%% length(U)==3\n"++
    "\n"++
    "-module("++?MODULENAME++").\n"++
    "\n"++
    ?INCLUDE_CMD_IN_DEST_MODULE++
    "\n"++
    "-export([get_index/2,get_key/2,get_value/2,length/1,fields_info/1]).\n"++
    "\n",
    %% Tail1="length(Other) -> exit({error,\"Invalid Record Name: \""++
    %% "++Other}).\n\n\n",
    Top++lists:flatten(Acc1).

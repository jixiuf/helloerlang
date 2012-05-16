-module(record_util2).
%%%-------------------------------------------------------------------
%%% File        : record_util.erl
%%% Author      : Gordon Guthrie gordon@hypernumbers.com
%%% Description : utilities for manipulating records
%%%
%%% Created     :  2 Sep 2008 by Gordon Guthrie
%%%-------------------------------------------------------------------
%% http://trapexit.org/Match%5FSpecifications%5FAnd%5FRecords%5F%28Dynamically!%29
%% http://forum.trapexit.org/viewtopic.php?p=21790
%% -include("myhead.hrl").
-define(HEAD_FILE_PATH,"../src/myhead.hrl").
-define(MODULENAME,"myhead_util2").

-export([make/0]).


make() ->
    {ok,Tree}=epp:parse_file(?HEAD_FILE_PATH,["./"],[]),
    Src=make_src(Tree),
    ok=file:write_file(?MODULENAME++".erl",list_to_binary(Src)).

make_src(Tree) -> make_src(Tree,[]).

make_src([],Acc)                              ->
    top_and_tail([make_index(Acc,[]),
                  "\n",
                  make_key(Acc,[]),
                  "\n",
                  make_length(Acc,[])]);
make_src([{attribute,_,record,Record}|T],Acc) -> make_src(T,[Record|Acc]);
make_src([_H|T],Acc)                          -> make_src(T,Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_index([],Acc1)    ->
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

mk_get_index(Name,Field,N) ->
    "get_index("++atom_to_list(Name)++","++
    atom_to_list(Field)++")-> "++integer_to_list(N)++";\n".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_key([],Acc1)    ->
    lists:flatten(Acc1)++
        "get_key(Record,_Index) -> exit({error,\""++
        "Invalid Record Name: \"++Record}).\n";
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
        " has no field called \"++atom_to_list(Index)});\n".

mk_get_key(Name,Field,N) ->
    "get_key("++atom_to_list(Name)++","++
    integer_to_list(N)++")-> "++atom_to_list(Field)++";\n".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_length([],Acc1)->
    Tail1="no_of_fields(Other) -> exit({error,\"Invalid Record Name: \""++
    "++Other}).\n\n\n",
    lists:reverse([Tail1|Acc1]);
make_length([{RecName,Def}|T],Acc1)->
    Cause= "no_of_fields("++atom_to_list(RecName)++") -> "++
           integer_to_list(length(Def))++";\n",
    make_length(T,[Cause|Acc1])
    .

top_and_tail(Acc1)->
    Top="%% This module automatically generated - do not edit\n"++
    "\n"++
    "%%% This module provides utilities for use in building\n"++
    "%%% match specifications from records\n"++
    "\n"++
    "-module("++?MODULENAME++").\n"++
    "\n"++
    "-export([get_index/2,get_key/2,no_of_fields/1]).\n"++
    "\n",
    %% Tail1="no_of_fields(Other) -> exit({error,\"Invalid Record Name: \""++
    %% "++Other}).\n\n\n",
    Top++lists:flatten(Acc1).

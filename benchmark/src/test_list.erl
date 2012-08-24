-module(test_list).
-export([test/1]).
-define(CONFIG_75_ELEMENT,"../75.json").
-define(CONFIG_100_ELEMENT,"../100.json").
-define(CONFIG_200_ELEMENT,"../200.json").
-define(CONFIG_300_ELEMENT,"../300.json").
-define(CONFIG_500_ELEMENT,"../500.json").
-define(CONFIG_1000_ELEMENT,"../1000.json").

test(Count)->
    io:format("75 elements in list,query count=~p,list,    time=~p ~n",
              [Count,test_list(Count*3,?CONFIG_75_ELEMENT,[<<"1">>,<<"30">>,<<"75">>])]),
    io:format("count=~p,dict     time=~p ~n",[Count,test_dict(Count)]),
    io:format("count=~p,orddict  time=~p ~n",[Count,test_orddict(Count)])
    .

test_list(Count,JsonFile,Keys)->
    DataList=init_list(JsonFile),
    timer:tc(fun()-> list(DataList,Count,Keys)end)
    .
init_list(JsonFile)->
    {ok,JsonData}=file:read_file(JsonFile),
    {struct,DataList}=mochijson2:decode(JsonData),
    DataList.

list(_List,0,_Keys)->
    ok;
list(List,Count,Keys)->
    %% lists:keyfind(<<"100">>,1,List),
    [lists:keyfind(Key,1,List)||Key<-Keys],
    list(List,Count-1,Keys).


test_dict(Count,JsonFile)->
    DataList=init_dict(JsonFile),
    timer:tc(fun()-> test_dict(DataList,Count)end)
    .
test_orddict(Count,JsonFile)->
    DataList=init_orddict(JsonFile),
    timer:tc(fun()-> test_orddict(DataList,Count)end)
    .

init_dict(JsonFile)->
    List=init_list(JsonFile),
    dict:from_list(List).

test_dict(_List,0)->
    ok;
test_dict(Dict,Count)->
    dict:find(<<"1">>,Dict),
    dict:find(<<"100">>,Dict),
    dict:find(<<"200">>,Dict) ,
    dict:find(<<"300">>,Dict),
    test_dict(Dict,Count-1)
        .

init_orddict(JsonFile)->
    List=init_list(JsonFile),
    orddict:from_list(List).

test_orddict(_List,0)->
    ok;
test_orddict(Dict,Count)->
    orddict:find(<<"1">>,Dict),
    orddict:find(<<"100">>,Dict),
    orddict:find(<<"200">>,Dict) ,
    orddict:find(<<"300">>,Dict),
    test_orddict(Dict,Count-1)
        .

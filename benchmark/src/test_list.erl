-module(test_list).
-export([test/1]).

test(Count)->
    io:format("count=~p,list,    time=~p ~n",[Count,test_list(Count)]),
    io:format("count=~p,dict     time=~p ~n",[Count,test_dict(Count)]),
    io:format("count=~p,orddict  time=~p ~n",[Count,test_orddict(Count)])
    .

test_list(Count)->
    DataList=init_list(),
    timer:tc(fun()-> test_list(DataList,Count)end)
    .


test_dict(Count)->
    DataList=init_dict(),
    timer:tc(fun()-> test_dict(DataList,Count)end)
    .
test_orddict(Count)->
    DataList=init_orddict(),
    timer:tc(fun()-> test_orddict(DataList,Count)end)
    .

init_list()->
    {ok,JsonData}=file:read_file("../test_list.json"),
    {struct,DataList}=mochijson2:decode(JsonData),
    DataList.

test_list(_List,0)->
    ok;
test_list(List,Count)->
    lists:keyfind(<<"1">>,1,List),
    lists:keyfind(<<"100">>,1,List),
    lists:keyfind(<<"200">>,1,List) ,
    lists:keyfind(<<"300">>,1,List),
    test_list(List,Count-1)
        .
init_dict()->
    List=init_list(),
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

init_orddict()->
    List=init_list(),
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

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
              [3*Count,test_list(Count,?CONFIG_75_ELEMENT,[<<"1">>,<<"30">>,<<"75">>])]),
    io:format("100 elements in list,query count=~p,list,    time=~p ~n",
              [3*Count,test_list(Count,?CONFIG_100_ELEMENT,[<<"1">>,<<"50">>,<<"100">>])]),
    io:format("200 elements in list,query count=~p,list,    time=~p ~n",
              [3*Count,test_list(Count,?CONFIG_200_ELEMENT,[<<"1">>,<<"100">>,<<"200">>])]),
    io:format("300 elements in list,query count=~p,list,    time=~p ~n",
              [3*Count,test_list(Count,?CONFIG_300_ELEMENT,[<<"1">>,<<"150">>,<<"300">>])]),
    io:format("500 elements in list,query count=~p,list,    time=~p ~n",
              [3*Count,test_list(Count,?CONFIG_500_ELEMENT,[<<"1">>,<<"250">>,<<"500">>])]),
    io:format("1000 elements in list,query count=~p,list,    time=~p ~n",
              [3*Count,test_list(Count,?CONFIG_1000_ELEMENT,[<<"1">>,<<"500">>,<<"1000">>])]),
    io:format("~n",[]) ,
    io:format("75 elements in dict,query count=~p,dict,    time=~p ~n",
              [3*Count,test_dict(Count,?CONFIG_75_ELEMENT,[<<"1">>,<<"30">>,<<"75">>])]),
    io:format("100 elements in dict,query count=~p,dict,    time=~p ~n",
              [3*Count,test_dict(Count,?CONFIG_100_ELEMENT,[<<"1">>,<<"50">>,<<"100">>])]),
    io:format("200 elements in dict,query count=~p,dict,    time=~p ~n",
              [3*Count,test_dict(Count,?CONFIG_200_ELEMENT,[<<"1">>,<<"100">>,<<"200">>])]),
    io:format("300 elements in dict,query count=~p,dict,    time=~p ~n",
              [3*Count,test_dict(Count,?CONFIG_300_ELEMENT,[<<"1">>,<<"150">>,<<"300">>])]),
    io:format("500 elements in dict,query count=~p,dict,    time=~p ~n",
              [3*Count,test_dict(Count,?CONFIG_500_ELEMENT,[<<"1">>,<<"250">>,<<"500">>])]),
    io:format("1000 elements in dict,query count=~p,dict,    time=~p ~n",
              [3*Count,test_dict(Count,?CONFIG_1000_ELEMENT,[<<"1">>,<<"500">>,<<"1000">>])]),

    io:format("~n",[]) ,
    io:format("75 elements in orddict,query count=~p,orddict,    time=~p ~n",
              [3*Count,test_orddict(Count,?CONFIG_75_ELEMENT,[<<"1">>,<<"30">>,<<"75">>])]),
    io:format("100 elements in orddict,query count=~p,orddict,    time=~p ~n",
              [3*Count,test_orddict(Count,?CONFIG_100_ELEMENT,[<<"1">>,<<"50">>,<<"100">>])]),
    io:format("200 elements in orddict,query count=~p,orddict,    time=~p ~n",
              [3*Count,test_orddict(Count,?CONFIG_200_ELEMENT,[<<"1">>,<<"100">>,<<"200">>])]),
    io:format("300 elements in orddict,query count=~p,orddict,    time=~p ~n",
              [3*Count,test_orddict(Count,?CONFIG_300_ELEMENT,[<<"1">>,<<"150">>,<<"300">>])]),
    io:format("500 elements in orddict,query count=~p,orddict,    time=~p ~n",
              [3*Count,test_orddict(Count,?CONFIG_500_ELEMENT,[<<"1">>,<<"250">>,<<"500">>])]),
    io:format("1000 elements in orddict,query count=~p,orddict,    time=~p ~n",
              [3*Count,test_orddict(Count,?CONFIG_1000_ELEMENT,[<<"1">>,<<"500">>,<<"1000">>])])
    %% io:format("count=~p,dict     time=~p ~n",[Count,test_dict(Count)]),
    %% io:format("count=~p,orddict  time=~p ~n",[Count,test_orddict(Count)])
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


test_dict(Count,JsonFile,Keys)->
    DataList=init_dict(JsonFile),
    timer:tc(fun()-> dict(DataList,Count,Keys)end)
    .
test_orddict(Count,JsonFile,Keys)->
    DataList=init_orddict(JsonFile),
    timer:tc(fun()-> orddict(DataList,Count,Keys)end)
    .

init_dict(JsonFile)->
    List=init_list(JsonFile),
    dict:from_list(List).

dict(_List,0,_Keys)->
    ok;
dict(Dict,Count,Keys)->
    [dict:find(Key,Dict)||Key<-Keys],
    dict(Dict,Count-1,Keys).

init_orddict(JsonFile)->
    List=init_list(JsonFile),
    orddict:from_list(List).

orddict(_List,0,_Keys)->
    ok;
orddict(Dict,Count,Keys)->
    [orddict:find(Key,Dict)||Key<-Keys],
    orddict(Dict,Count-1,Keys)
        .

-module(test_list_dict_orddict2).
-export([test/1]).
%% 实践证明 ，dict在插入方面性能很差
%% test list 10000,time:1035
%% test dict 10000,time:19276
%% test dict(by dict:from_list) 10000,time:18049
%% test orddict 10000,time:867


test(Count)->
    test_list(Count),
    test_dict(Count),
    test_dict2(Count),
test_orddict(Count)
    .

test_list(Count)->
    {Time,_}=
        timer:tc(fun()->
                         insert_list(Count,[])
                 end),
    io:format("test list ~p,time:~p~n",[Count,Time])
    .
insert_list(0,List)->List;
insert_list(Count,List) ->insert_list(Count-1,[{Count,Count}|List]).

test_dict(Count)->
    D=dict:new(),
    {Time,_}=timer:tc(fun()->
                              insert_dict(Count,D)
                      end),
io:format("test dict ~p,time:~p~n",[Count,Time])
        .
insert_dict(0,Dict)->
    Dict;
insert_dict(Count,Dict)->
    D2=dict:store(Count,Count,Dict),
    insert_dict(Count-1,D2)
        .


test_dict2(Count)->
    {Time,_}=
        timer:tc(fun()->
                         dict:from_list(insert_list(Count,[]))
                 end),
io:format("test dict(by dict:from_list) ~p,time:~p~n",[Count,Time])
        .

test_orddict(Count)->
    D=orddict:new(),
    {Time,_}=timer:tc(fun()->
                              insert_orddict(Count,D)
                      end),
    io:format("test orddict ~p,time:~p~n",[Count,Time])
.
insert_orddict(0,Dict)->
    Dict;
insert_orddict(Count,Dict)->
    D2=orddict:store(Count,Count,Dict),
    insert_orddict(Count-1 ,D2)
    .

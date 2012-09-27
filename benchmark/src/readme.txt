http://www.ostinelli.net/boost-message-passing-between-erlang-nodes/
测试 两节点间 send receive 消息需要多长时间

erl -sname pong
start_pong:start().

erl -name ping

start_ping:start('pong@192.168.1.31',1000). %send 1000条
** test_list_dict_orddict.erl 测list dict orddict的查询速度
%% 就查询速度而言，dict是三者中最好的,不论dict中元素个数多少，速度很稳定,
%% 200个元素以下，list 似乎比orddict要好，超过这个数orddict略胜， 但orddict list 均不如 dict
%% 书上说75个元素时 orddict效率最高，但好像即便是75个元素orddict也比不过list(就查询而言)
%% 75 elements in list,query count=300,list,    time={266,ok}
%% 100 elements in list,query count=300,list,    time={353,ok}
%% 200 elements in list,query count=300,list,    time={694,ok}
%% 300 elements in list,query count=300,list,    time={2035,ok}
%% 500 elements in list,query count=300,list,    time={1730,ok}
%% 1000 elements in list,query count=300,list,    time={3609,ok}

%% 75 elements in dict,query count=300,dict,    time={104,ok}
%% 100 elements in dict,query count=300,dict,    time={112,ok}
%% 200 elements in dict,query count=300,dict,    time={112,ok}
%% 300 elements in dict,query count=300,dict,    time={113,ok}
%% 500 elements in dict,query count=300,dict,    time={94,ok}
%% 1000 elements in dict,query count=300,dict,    time={111,ok}

%% 75 elements in orddict,query count=300,orddict,    time={624,ok}
%% 100 elements in orddict,query count=300,orddict,    time={323,ok}
%% 200 elements in orddict,query count=300,orddict,    time={708,ok}
%% 300 elements in orddict,query count=300,orddict,    time={1724,ok}
%% 500 elements in orddict,query count=300,orddict,    time={3777,ok}
%% 1000 elements in orddict,query count=300,orddict,    time={2684,ok}

%% 实践证明 ，dict在插入方面性能很差
%% test list 10000,time:1035
%% test dict 10000,time:19276
%% test dict(by dict:from_list) 10000,time:18049
%% test orddict 10000,time:867
dict适合读多与写的情况,

-module(node_test).
-export([test5/0,test4/0,test3/0,test2/0,test/0]).
%% 每个节点上开启多少个mysql连接
%% {ok,P}=emysql_node:start_link().
%%
%% emysql_node:execute(P,fun test/0).
-record(table_1,{id,name}).
%% create table table_1(id int primary key  auto_increment ,name varchar(255));
test()->
    emysql_node:assert(),
    emysql_node:execute(emysql_node,
                        fun()->
                                A=emysql:execute(hello_pool, <<"select id,name from table_1 limit 1,10">>),
                                C=emysql_util:as_record(A,table_1,record_info(fields,table_1)),
                                io:format("~p~n",[C]),

                                emysql:execute(hello_pool, <<"INSERT INTO table_1 SET name = 'Hello Worlddddddd!'">>)

                        end
                       ).

test2()->
    emysql_node:assert(),
    emysql_node:execute(emysql_node,
                        fun()->
                                emysql:prepare(my_stmt, <<"SELECT * from table_1 WHERE id = ?">>),
                                emysql:execute(hello_pool, my_stmt, [1])
                        end
                       ).

test3()->
    emysql_node:assert(),
    emysql_node:execute(emysql_node,
                        fun()->
                                emysql:execute(hello_pool, <<"create procedure my_sp() begin select * from table_1; end">>),
                                emysql:execute(hello_pool, <<"call my_sp();">>)
                        end
                       ).

%% tranasction test
test4()->
    emysql_node:assert(),
    emysql_node:execute(emysql_node,
                        fun()->
                                emysql:transaction(hello_pool,
                                                   fun(C)->
                                                           emysql_conn:execute(C, <<"update table_1 set name='aaaa'">>,[]),
                                                           emysql_conn:execute(C, <<"select * from table_1">>,[])
                                                   end)
                        end
                       ).

test5()->
    emysql_node:assert(),
    emysql_node:execute(emysql_node,
                        fun()->
                                emysql:transaction(hello_pool,
                                                   fun(C)->
                                                           emysql_conn:execute(C, <<"update table_1 set name='bbbbbbb'">>,[]),
                                                           exit(baddddddddd),
                                                           emysql_conn:execute(C, <<"select * from table_1">>,[])
                                                   end)
                        end
                       ).

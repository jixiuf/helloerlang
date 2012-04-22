-module(node2_test).
-export([test2/0,loop/1,test1/0,test/0]).
-include("tbl_fileds.hrl").
-define(listener,emysql_center).
-record(table_1,{id,name}).
%% create table table_1(id int primary key  auto_increment ,name varchar(255));

test()->
    emysql_center:assert(),                     %run on server side ,make sure server is started
    NodeList= emysql_center:get_all_mysql_proxy_node(),
    Node=lists:nth(random:uniform(length(NodeList)),NodeList),
    D=emysql_center:execute(Node,
                            fun()->
                                    A=emysql:execute(hello_pool, <<"select id,name from table_1 limit 1,10">>),
                                    C=emysql_util:as_record(A,table_1,record_info(fields,table_1)),
                                    io:format("~p~n",[C]),

                                    emysql:execute(hello_pool, <<"INSERT INTO table_1 SET name = 'Hello Worlddddddd!'">>)

                            end
                           ),
    io:format("he:~p~n",[D])
    %% Pid=global:whereis_name(emysql_center)
        .

%% emysql_center:assert().                     %run on server side ,make sure server is started

%% net_adm:ping('emacs@192.168.0.44').
%% node2_test:test1().
test1()->
    NodeList= emysql_center:get_all_mysql_proxy_node(),
    Node=lists:nth(random:uniform(length(NodeList)),NodeList),
    emysql_center:execute(Node,
                          fun()->
                                  %% A=emysql:execute(hello_pool, <<"select id,name from table_1 limit 1,10">>),
                                  %% C=emysql_util:as_record(A,table_1,record_info(fields,table_1)),
                                  %% io:format("~p~n",[C]),

                                  emysql:execute(hello_pool, <<"INSERT INTO table_1 SET name = 'Hello Worlddddddd!'">>)

                          end
                         )
    %% Pid=global:whereis_name(emysql_center)
        .

%% emysql_center:assert().                     %run on server side ,make sure server is started
%% net_adm:ping('emacs@192.168.0.44').
%% node2_test:loop(100).
loop(0)->
    ok;
loop(N)->
    test1(),
    spawn(fun()->loop(N-1)end),
    loop(N-1)
        .

%% net_adm:ping('emacs@192.168.0.44').
%% node2_test:test2().
test2()->
    net_kernel:connect_node('emacs@192.168.0.44'),
    NodeList= emysql_center:get_all_mysql_proxy_node(),
    Node=lists:nth(random:uniform(length(NodeList)),NodeList),

    OnOffRecord= #tbl_onoff{
      onOffType=1,
      accountId=1,
      accountType=1,
      playerId=1,
      playerLevel=1,
      playerName="name",
      datetime=123,
      clientVersion="ver",
      clientType="t",
      issuers="dd",
      flashPlayerVersion="dd",
      connectType="con",
      gameServerName="name"
     },

        %% Result3 = emysql:execute(test_pool,<<   "call addOnOff();"        >>),

    emysql_center:send_log(Node,    OnOffRecord)
    %% Pid=global:whereis_name(emysql_center)
        .

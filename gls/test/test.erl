-module(test).
-export([test9/0,test8/0,test7/0,test6/0,test5/0,test4/0,test3/0,test2/0]).
-include("tbl_fileds.hrl").
%% -define(listener,emysql_center).
%% -record(table_1,{id,name}).
%% create table table_1(id int primary key  auto_increment ,name varchar(255));

%% test()->
%%     emysql_center:assert(),                     %run on server side ,make sure server is started
%%     NodeList= emysql_center:get_all_mysql_proxy_node(),
%%     Node=lists:nth(random:uniform(length(NodeList)),NodeList),
%%     D=emysql_center:execute(Node,
%%                             fun()->
%%                                     A=emysql:execute(hello_pool, <<"select id,name from table_1 limit 1,10">>),
%%                                     C=emysql_util:as_record(A,table_1,record_info(fields,table_1)),
%%                                     io:format("~p~n",[C]),

%%                                     emysql:execute(hello_pool, <<"INSERT INTO table_1 SET name = 'Hello Worlddddddd!'">>)

%%                             end
%%                            ),
%%     io:format("he:~p~n",[D])
%%     %% Pid=global:whereis_name(emysql_center)
%%         .

%% %% emysql_center:assert().                     %run on server side ,make sure server is started

%% %% net_adm:ping('emacs@192.168.0.44').
%% %% node2_test:test1().
%% test1()->
%%     NodeList= emysql_center:get_all_mysql_proxy_node(),
%%     Node=lists:nth(random:uniform(length(NodeList)),NodeList),
%%     emysql_center:execute(Node,
%%                           fun()->
%%                                   %% A=emysql:execute(hello_pool, <<"select id,name from table_1 limit 1,10">>),
%%                                   %% C=emysql_util:as_record(A,table_1,record_info(fields,table_1)),
%%                                   %% io:format("~p~n",[C]),

%%                                   emysql:execute(hello_pool, <<"INSERT INTO table_1 SET name = 'Hello Worlddddddd!'">>)

%%                           end
%%                          )
%%     %% Pid=global:whereis_name(emysql_center)
%%         .

%% %% emysql_center:assert().                     %run on server side ,make sure server is started
%% %% net_adm:ping('emacs@192.168.0.44').
%% %% node2_test:loop(100).
%% loop(0)->
%%     ok;
%% loop(N)->
%%     test1(),
%%     spawn(fun()->loop(N-1)end),
%%     loop(N-1)
%%         .

%% net_adm:ping('emacs@192.168.0.44').
%% node2_test:test2().
%% rr(node2_test).
%%
test9()->
    net_kernel:connect_node('emacs@192.168.11.157'),

    NodeList= game_log_server:get_all_mysql_proxy_node(),
    Node=game_log_server:get_random_node(NodeList),
    Record= #tbl_reg{%
      accountType         =1,
      playerId            =1,
      playerName          =1,
      logTime            ="1989-01-26",
      clientVersion       =1,
      clientType          =1,
      issuers             =1,
      flashPlayerVersion  =1,
      connectType         =1,
      gameServerName      =1,
      ipAddr              =1,
      accountId           =1
     },
    %% Result3 = emysql:execute(test_pool,<<   "call addOnOff();"        >>),

    game_log_server:send_log(Node,    Record)
    %% Pid=global:whereis_name(emysql_center)
        .
test8()->
    net_kernel:connect_node('emacs@192.168.11.157'),

    NodeList= game_log_server:get_all_mysql_proxy_node(),
    Node=game_log_server:get_random_node(NodeList),
    Record= #tbl_onlinenum{%
      onlinenum=1,
      gameServerName=1,
      logTime            ="1989-01-26"
     },
    %% Result3 = emysql:execute(test_pool,<<   "call addOnOff();"        >>),

    game_log_server:send_log(Node,    Record)
    %% Pid=global:whereis_name(emysql_center)
        .

test7()->
    net_kernel:connect_node('emacs@192.168.11.157'),

    NodeList= game_log_server:get_all_mysql_proxy_node(),
    Node=game_log_server:get_random_node(NodeList),
    Record= #tbl_item_change{%
      accountType                     =1,
      playerId                        =1,
      playerName                      =1,
      itemId                          =1,
      itemTid                         =1,
      itemPosId                       =1,
      extList                         =1,
      oT_OLDEQUIP_ID                  =1,
      oT_OLDEQUIP_SOURCE_DATA_INDEX   =1,
      oT_ACTION_TYPE                  =1,
      oT_ACTION_TIME                  ="01:01:01",
      oT_EQUIPMENT_MAP                =1,
      oT_EQUIPMENT_NOTE               =1,
      oT_ISSUERS_ID                   =1,
      oT_GAME_SERVER_NAME             =1,
      oT_GAME_ZONE_NAME               =1,
      account                         =1

     },

    %% Result3 = emysql:execute(test_pool,<<   "call addOnOff();"        >>),

    game_log_server:send_log(Node,    Record)
    %% Pid=global:whereis_name(emysql_center)
        .
test6()->
    net_kernel:connect_node('emacs@192.168.11.157'),

    NodeList= game_log_server:get_all_mysql_proxy_node(),
    Node=game_log_server:get_random_node(NodeList),
    Record= #tbl_consume_item{%
      accountType           =1,
      playerId              =1,
      playerName            =1,
      consumeItem           =1,
      consumeCount          =1,
      itemType              =1,
      itemTid               =1,
      itemId                =1,
      itemSellCoin          =1,
      logTime            ="1989-01-26",
      mapId                 =1,
      sellNpcId             =1,
      clientVersion         =1,
      clientType            =1,
      issuers               =1,
      flashPlayerVersion    =1,
      connectType           =1,
      gameServerName        =1,
      ipAddr                =1,
      accountId             =1

     },

    %% Result3 = emysql:execute(test_pool,<<   "call addOnOff();"        >>),

    game_log_server:send_log(Node,    Record)
    %% Pid=global:whereis_name(emysql_center)
        .
test5()->
    net_kernel:connect_node('emacs@192.168.11.157'),

    NodeList= game_log_server:get_all_mysql_proxy_node(),
    Node=game_log_server:get_random_node(NodeList),
    Record= #tbl_add_item{%
      accountType=1,
      playerId=1,
      playerName             =1, %
      addItemType            =1, % 角色
      itemCount              =1, %
      itemType               =1, %
      itemTid                =1, %
      itemId                 =1, %
      needCoin               =2, %
      needMoney              =3, %
      logTime            ="1989-01-26",
      mapId                  =2, %
      giverId                =3, %
      taskId                 =2, %
      addItemResult          =23, %
      clientVersion          =2, %
      clientType             ="dd",
      issuers                ="dd",
      flashPlayerVersion    ="con",
      connectType            ="name",
      gameServerName         ="ip",
      ipAddr                 =1, %
      accountId=2                },

    %% Result3 = emysql:execute(test_pool,<<   "call addOnOff();"        >>),

    game_log_server:send_log(Node,    Record)
    %% Pid=global:whereis_name(emysql_center)
        .
test4()->


    net_kernel:connect_node('emacs@192.168.11.157'),

    NodeList= game_log_server:get_all_mysql_proxy_node(),
    Node=game_log_server:get_random_node(NodeList),
    Record= #tbl_add_gold{%
      accountType         =1, %
      playerId            =1, % 角色
      playerName          ="asdf", %
      addGoldType         =1, %
      addMoney            =1, %
      addCoin             =1, %
      giverId             =2, %
      taskId              =3, %
      logTime            ="1989-01-26",
      mapId               =2, %
      sellItemType        =3, %
      sellItemTid         =2, %
      sellItemId          =23, %
      clientVersion       =2, %
      clientType          ="dd",
      issuers             ="dd",
      flashPlayerVersion  ="con",
      gameServerName      ="name",
      ipAddr              ="ip",
      accountId           =1 %
     },

    %% Result3 = emysql:execute(test_pool,<<   "call addOnOff();"        >>),

    game_log_server:send_log(Node,    Record)
    %% Pid=global:whereis_name(emysql_center)
        .
test3()->


    net_kernel:connect_node('emacs@192.168.11.157'),

    NodeList= game_log_server:get_all_mysql_proxy_node(),
    Node=game_log_server:get_random_node(NodeList),
    Record= #tbl_add_exp{%
      accountType=1, % 用户账号类型
      playerId=1, % 角色ID
      playerName="asdf", % 角色昵称
      addExpType=1, % 获得经验的方式
      isLevelUp=1, % 等级是否有变化
      oldLevel=1, % 增加经验以前等级
      newLevel=2, % 增加经验以后等级
      addExp=3, % 获得经验数
      giverId=3, % NPCID和掉落经验的怪物ID,GM角色ID
      taskId=2, % 获取奖励经验的任务和活动ID
      logTime            ="1989-01-26",
      mapId=2, % 用户获得经验的地图
      clientVersion=23, % 客户端版本
      clientType=2, % 客户端平台类型
      issuers="dd",
      flashPlayerVersion="dd",
      connectType="con",
      gameServerName="name",
      ipAddr="ip",
      accountId=1 % 用户账号
     },

    %% Result3 = emysql:execute(test_pool,<<   "call addOnOff();"        >>),

    game_log_server:send_log(Node,    Record)
    %% Pid=global:whereis_name(emysql_center)
        .
test2()->
    %% 客户端 标准做法是:
    %% 1,connect node ,只需要做一次，不用每次都调用
    %% net_kernel:connect_node('emacs@192.168.0.44'),
    %% 此处为测试方便 写到本测试方法内部
    %%net_kernel:connect_node('emacs@192.168.0.44'),
    %% 2.从服务器获得可用的mysql节点列表
    %% NodeList= game_log_server:get_all_mysql_proxy_node(),
    %% 3.调用统一接口,game_log_server:get_random_node(NodeList),
    %% 从众多的mysql 列表中随机获得一个
    %% 此方法 在客户端执行,未访问服务器，
    %%4.    game_log_server:send_log(Node,    OnOffRecord)



    net_kernel:connect_node('emacs@192.168.1.108'),

    NodeList= game_log_server:get_all_mysql_proxy_node(),
    Node=game_log_server:get_random_node(NodeList),

    OnOffRecord= #tbl_on_off{
      onOffType=1,
      accountId=1,
      accountType=1,
      playerId=1,
      headHeroId=1,
      playerLevel=1,
      playerName="name",
      logTime="1989-01-26", % 用户获得经验的时间
      clientVersion="ver",
      clientType="t",
      issuers="dd",
      flashPlayerVersion="dd",
      connectType="con",
      gameServerName="name",
      ipAddr="localhost"%IP地址
     },

    %% Result3 = emysql:execute(test_pool,<<   "call addOnOff();"        >>),

    game_log_server:send_log(Node,    OnOffRecord)
    %% Pid=global:whereis_name(emysql_center)
        .

-module(db).
-export([create_table/0,insert/6,query_user/1,user_add_age/1,select_user_by_name/1,select_user_by_name2/1,select_all_user/0]).
-export([qlc_query_user/1]).

-record(user,{id,name,age}).
-record(group,{id,name,desc}).
-record(user_group_relation,{userid,groupid}).

-include_lib("stdlib/include/qlc.hrl").
%%调用之前，需 mnesia:create_schema([node()]).
%% mnesia:start().
%%% db:create_table()
create_table()->
    %%   {type, Type}这里Type必须是set, ordered_set 或bag这三个原子之一，默认值为
    %%   set。注意：目前'ordered_set'不支持'disc_only_copies'表。set或
    %%   ordered_set类型的表每个键只能有零或一条记录，而bag类型的表每个键可以有任
    %%   意数量的记录。每条记录的键总是记录的第一个属性。

    %% {disc_copies, NodeList}，这里NodeList是表要存储在磁盘上的节点列表。默认值
    %%    是 []。
    %% {ram_copies, NodeList}，这里NodeList是表要存储在内存中的节点列表，默认值
    %%    是 [node()]。如果采用默认值创建新表，将仅仅位于本地节点
    %% {disc_only_copies, NodeList}，这种类型的表副本只存储在硬盘上，因此访问比较
    %%    慢，但是这种类型的表比其它两种类型的表消耗的内存要少

    %% {index, AttributeNameList}，这里AttributeNameList是一个原子类型的属性名列表，
    %% Mnesia将对其指定的属性建立和维护索引，列表中的每个元素都有一个索引表。
    %% Mnesia记录的第一个域是键，所以不需要建立额外的索引。

    %% {attributes,AttributeNameList}表字段名，第一个为key

    %% {record_name, Atom}指定表中所有记录的通用名，全部存储在表中的记录都必须
    %%     用这个名字作为其第一个元素。默认的记录名是表的名字。更多信息参见第4章：
    %%     记录名与表名。

    %% {snmp, SnmpStruct}. SnmpStruct在SNMP用户指南中描述。如果在函数
    %% mnesia:create_table/2的参数ArgList中出现这个属性，表示该表可以立即通过简单
    %% 网络管理协议(SNMP)来访问。
    %% 可以很容易设计出使用SNMP操作和控制系统的应用程序。Mnesia提供由逻辑表
    %% 构成的SNMP控制应用程序与由物理数据构成的Mnesia表之间的直接映射。默
    %% 认值为[]。

    %% {local_c%% ontent, true}，当应用需要一个其内容对每个节点来说在本地都是唯一的表
    %% 时，可使用local_content表。表名对所有Mnesia节点可见，但是内容对每个节点
    %% 都是唯一的。这种类型的表只能在本地进行存取。


    %% record_info(fields, RecordName)表达式由Erlang预处理程序处理为一个包含一个record的相同fields的list
    %% mnesia:create_table(user,[{type,bag},{attributes,[id,name,age]}]), %bag 允许重复数据
    mnesia:create_table(user,[{type,bag},{attributes,record_info(fields ,user)}]), %bag 允许重复数据
    mnesia:create_table(group,[{type,set},{attributes,record_info(fields ,group)}]), %set 不允许重复数据
    mnesia:create_table(user_group_relation,[{attributes,record_info(fields ,user_group_relation)}])
        .
%%% insert()
 %% db:insert(1,"jixf",11,1,"group","group ").
insert(UserId,UserName,UserAge,GroupId,GroupName,GroupDesc)->
    io:format("insert data to tables...~n",[]),
    Fun = fun() ->
                  insert_user(UserId,UserName,UserAge),
                  insert_group(GroupId,GroupName,GroupDesc),
                  insert_user_group_relation(UserId,GroupId)
          end,
    mnesia:transaction(Fun)
        .

insert_user(Id,Name,Age)->
                  User=#user{id=Id,name=Name,age=Age},
                  mnesia:write(User)
        .
insert_group(Id,Name,Desc)->
                  Group=#group{id=Id,name=Name,desc=Desc},
                  mnesia:write(Group)
        .
insert_user_group_relation(UserId,GroupId)->
    User_Group_Relation=#user_group_relation{userid=UserId,groupid=GroupId},
    mnesia:write(User_Group_Relation)
        .
%%% query by mnesia:select()
%% db:query_user().
%% mnesia:read({Tab,Key})
%% mnesia:read(Tab,Key)
%% mnesia:read(Tab,Key,read)
%% wread({Tab,Key})= read(Tab,Key,write)
query_user(UserId)->
    Fun = fun()->
                  Results=mnesia:read(user,UserId),  %参数{Tab,Key},似乎这个Key 是-record的第一个属性
                  lists:map(fun(Result)->
                                    io:format("userid=~p,username=~p,age=~p ~n",[1,Result#user.name,Result#user.age])
                            end,
                            Results)
          end,
    mnesia:transaction(Fun)
        .
%% 给指定的用户 age+1
%% wread({Tab,Key})= read(Tab,Key,write)
user_add_age(UserId)->
    Fun = fun()->
                  Results=mnesia:wread ({user,UserId}),  %参数{Tab,Key},似乎这个Key 是-record的第一个属性
                  lists:map(fun(Result)->
                                    io:format("old: userid=~p,username=~p,age=~p ~n",[1,Result#user.name,Result#user.age]),
                                    NewUser = Result#user{age=(Result#user.age+1 )},
                                    mnesia:write(NewUser) %此种操作方式并非update ,而是重新添加了一条新数据，未找到update语句
                            end,
                            Results)
          end,
    mnesia:transaction(Fun),
    query_user(UserId)
        .
%% 变态的select 语句
select_user_by_name(Username)->                 %select name ,age from user where name=?
    Fun = fun()->
                  MatchPattern=  #user{_='_',name='$1',age='$2' }, %相当于将name ,age绑定到$1, $2 上，下文中Guard,Result 可以引用之，
                  Guard=[{'==','$1', Username}],                   %$1 == Username 作为 判断条件
                  Result=[['$1','$2']],                            %结果，只取[$1 ,$2]作为返回值
                  Results=mnesia:select(user,[{MatchPattern,Guard,Result}]),
                  lists:map(fun([Name,Age])->
                                    io:format(" name=~p,age=~p ~n",[Name,Age])
                            end,
                            Results)
          end,
    mnesia:transaction(Fun)
        .
select_user_by_name2(Username)->                 %select name from user where name=?
    Fun = fun()->
                  MatchPattern=  #user{_='_',name='$1',age='$2' }, %相当于将name绑定到$1, 上，下文中Guard,Result 可以引用之，
                  Guard=[{'==','$1', Username}],                   %$1 == Username 作为 判断条件
                  Result=['$2'],                            %结果，只取$2作为返回值
                  Results=mnesia:select(user,[{MatchPattern,Guard,Result}]),
                  lists:map(fun(Age)->
                                    io:format(" name=~p ,age=~p ~n",[Username,Age])
                            end,
                            Results)
          end,
    mnesia:transaction(Fun)
        .
select_all_user()->
    Fun = fun()->
                  MatchPattern=  #user{_='_',name='$1',age='$2' }, %相当于将name绑定到$1, 上，下文中Guard,Result 可以引用之，
                  Guard=[],
                  Result=[['$1','$2']],                            %结果，只取$1,$2作为返回值  即name, age
                  Results=mnesia:select(user,[{MatchPattern,Guard,Result}]),
                  lists:map(fun([Name,Age])->
                                    io:format(" name=~p ,age=~p ~n",[Name,Age])
                            end,
                            Results)
          end,
    mnesia:transaction(Fun)
        .
%%% query by qlc 使用QLC可能比使用Mnesia方法开销更大，但是它提供了一个很好的语法
qlc_query_user(Username)->
    Fun= fun()->
                 io:format("query by qlc~n",[]),
                  Q=qlc:q([User|| User <- mnesia:table(user),User#user.name==Username]),
                 qlc:e(Q)
         end,
    {atomic,Result} =mnesia:transaction(Fun),
    lists:map(fun(U)->
                      io:format("userid=~p,username=~p,age=~p ~n",[U#user.id,U#user.name,U#user.age])
              end,
              Result)
        .


%%% Usage
%%调用之前，需 mnesia:create_schema([node()]).
%% mnesia:start().
%% db:create_table().
%% db:insert(1,"jixf",11,1,"group","group ").
%% db:insert(1,"jixf2",22,1,"group","group ").
%% db:query_user(1).
%% db:user_add_age(1).
%% db:select_user_by_name("jixf").
%% db:select_user_by_name2("jixf").
%% db:select_all_user().

-module(id_auto_increment).
-export([test/0]).

%% 像mysql 一样，主键递增
%% 方法mnesia:dirty_update_counter(Tab, Key, Incr)
%% Tab: 某个存放Key对应的最新键值的mnesia数据库表,这个表保存key-value
%% Key: 键名
%% Incr: 键值递增量

%% 这个方法的意义在于:
%% 1.它的效率很高;
%% 2.这个方法是原子操作,不管有没有在事务中使用,它总是一个原子操作
%% 如果两个进程同时调用此方法,每个调用都会得到正确的更新值.

%% 假设我们设计的t_user表需要有一个id,这个id是唯一且递增的,

%% 1.创建如下结构的mnesia数据库表
-record(unique_id, {item, uid}).

-record(t_user, {userid,username}).

%% 2.每为t_user表加入一条新记录时,需要得到新的id值:
%% mnesia:dirty_update_counter(unique_id, t_user, 1)
%% 这里我们把表名作为Key,通过此方法即得到这个表当前的id

%% create table
init()->
    mnesia:start(),
    mnesia:create_table(t_user,[{type,set},{attributes,record_info(fields ,t_user)}]),
    mnesia:create_table(unique_id,[{type,set},{attributes,record_info(fields ,unique_id)}]) %set 不允许重复数据
        .

test()->
    init(),
    Fun= fun()->
                 User_id =mnesia:dirty_update_counter(unique_id, t_user, 1),
                 User=#t_user{userid=User_id,username="jixf"},
                 mnesia:write(User)

         end,
    %% 向表中插入两条记录
    mnesia:transaction(Fun),
    mnesia:transaction(Fun)
        .


%% id_auto_increment:test().
%% 使用tv来查看mnesia 表中的记录(图形界面 )
%% tv:start().

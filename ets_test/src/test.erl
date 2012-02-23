-module(test).
-export([test_create2/0,test_create1/0]).

%% http://www.cnblogs.com/me-sa/archive/2011/08/11/erlang0007.html
-record(emp, {empno,     %Employee number as a string, the key
              surname,   %Surname of the employee
              givenname, %Given name of employee
              dept,      %Department one of {dev,sales,prod,adm}
              empyear}). %Year the employee was employed

%% ETS查询时间是常量,例外是如果使用ordered_set查询时间与logN成正比(N为存储的数据量)

%% ETS 存储数据的格式是Tuple,下面的测试代码中我们可以看到细节

%% ETS Table由进程创建,进程销毁ETS Table也随着销毁,在使用Shell做ETS实验的时候要
%% 注意一下,Table的拥有关系可以give_away 转交给其它进程

%% 一个Erlang节点的ETS表的数量是有限制的,默认是1400个表,在启动erlang节点之前修
%% 改 ERL_MAX_ETS_TABLES参数可以修改这个限制ejabberd社区站点上总结的性能调优中
%% 提到了这一点,点击这里查看:
%% http://www.ejabberd.im/tuning

%% ETS表不在GC的管理范围内，除非拥有它的进程死掉它才会终止；可以通过delete删除
%% 数据

%% 目前版本,insert和lookup操作都会导致对象副本的创建,insert和lookup时间对于set
%% bag duplicate_bag都是常量值与表大小无关.

%% 并发控制：所有针对一个对象的更新都被保证是原子的、隔离的：修改要么全部成功要
%% 么失败。也没有其它的中间结果被其它的进程使用。有些方法可以在处理多个对象的时
%% 候保证这种原子性和隔离性。

%% 在数据库术语中隔离级别被称作序列化，就好像所有隔离的操作一个接一个严格按照顺
%% 序执行。

%% 在遍历过程中,可以使用safe_fixtable来保证遍历过程中不出现错误,所有数据项只被
%% 访问一遍.用到逐一遍历的场景就很少，使用safe_fixtable的情景就更少。不过这个机
%% 制是非常有用的，

%% 还记得在.net中版本中很麻烦的一件事情就是遍历在线玩家用户列表.由于玩家登录退
%% 出的变化,这里的异常几乎是不可避免的.select match内部实现的时候都会使用
%% safe_fixtable

%% ets:new(emp_tab,[{keypos,#emp.empno},named_table,ordered_set]). 首先，介绍查
%% 看ets表中数据的工具，一个是图形界面的tv:start(). 另一个就是
%% ets:i(tabname_or_id) ets:all()列出所有ets表ets:info(tabname_or_id),占用内存
%% 等内容

%% ets跟mnesia不一样，似乎不需要与一个record 对应。
test_create1()->
    T=ets:new(emp_tab,[named_table,ordered_set]),
    ets:insert(T,{1,111,11111}),
    ets:insert(emp_tab,{2,22,222}),
    ets:insert(emp_tab,{3,33})
    %% 测试结果如下，其格式完全是{} ，不限定元素的数量
    %% (emacs@JIXF)5> ets:i(emp_tab).
    %% <1   > {1,111,11111}
    %% <2   > {2,22,222}
    %% <3   > {3,33}
    %% EOT  (q)uit (p)Digits (k)ill /Regexp -->q
    %% ok

        .
test_create2()->
    T=ets:new(t2,[named_table]),
    ets:insert(T,[{1,11},{2,222,22222}])
    %% ets:i(t2). %
        .

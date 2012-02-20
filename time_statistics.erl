-module(time_statistics).
-export([test_statistics/0,test_tc/0,job/0]).

%%
%% 二、timer工具
%% timer模块中有tc函数用来测试某函数的执行时间，调用
%% timer:tc(Mod, Fun, Args)
%% 即可测试Mod:Fun函数的执行时间，注意结果单位是微秒（μs，microsecond），而不是毫秒（ms，millisecond）
%% 1s=1000ms，1 ms=1000μs

%% 三、erlang:statistics函数
%% statistics函数可用来统计某段代码的执行时间
%% Java代码  收藏代码

%%     statistics(wall_clock),
%%     ...........待统计的代码
%%     {_, Time} = statistics(wall_clock),


test_statistics()->
    statistics(wall_clock),
    job(),
    {_,Time}=statistics(wall_clock),
    Time
        .
test_tc()->
    timer:tc(?MODULE,job,[])
        .

job()->
    timer:sleep(2000)
.

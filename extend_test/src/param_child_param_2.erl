-module(param_child_param_2,[A]).
-export([show/0]).
-extends(param_parent).
%%测试： 一个parameter module 继承另一个 parameter module
%% 注意 ，此时两个 module都为一个参数

show()->
    io:format("~p:~p~n",[?MODULE,A])
        .

%% (emacs@jf.org)4> A=param_child_param_2:new(cat).
%% {param_child_param_2,cat}                  ----------------这时结果证明，返回的是子模块
%% (emacs@jf.org)5> A:show().
%% param_child_param_2:cat

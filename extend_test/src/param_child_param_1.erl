-module(param_child_param_1,[A,B]).
-export([show/0]).
-extends(param_parent).
%%测试： 一个parameter module 继承另一个 parameter module
%% 注意子module 两个参数， param_parent则为 一个参数

show()->
    io:format("~p:~p,~p~n",[?MODULE,A,B])
        .
%%
%% (emacs@jf.org)11> A=param_child_param_1:new(cat).
%% {param_parent,cat}
%% (emacs@jf.org)12> A:show().
%% name is :cat
%% 这里的show ，调用的是param_parent:show/0


%% (emacs@jf.org)13> B=param_child_param_1:new(cat,dog).
%% {param_child_param_1,cat,dog}
%% (emacs@jf.org)14> B:show().
%% ?MODULE:cat,dog

%% 证明  一个parameter module 可以继承另一个 parameter module

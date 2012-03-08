-module(param_child_normal).
%%测试 一个正常 的module 续承 参数化模块param_parent
-extends(param_parent).

%% B=param_child_normal:new(cat).
%% B:show().
%% 结果:
%% name is :cat
%% 证明:
%% 一个正常 的module 可以续承 参数化模块param_parent

%% 但是不可以这样调用param_child_normal:show()

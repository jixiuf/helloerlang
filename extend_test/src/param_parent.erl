-module(param_parent,[Name]).
-export([show/0]).
%% 此类为一参数化module
%% A=param_parent:new(cat).
%% A:show().
show()->
    io:format("name is :~p~n",[Name]).

-module(child).

%% 看这里
%% 只能单继承，
%% 可以A->B->C->D
-extends(parent).

-export([m1/1]).


m1(P)->
    io:format("fun child:m1/1 and param is ~p~n",[P])
        .

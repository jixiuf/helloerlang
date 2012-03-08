-module(parent).
-export([m1/1,m/0]).


m()->
    io:format("fun parent:m/0 ~n",[])
    .

m1(Param)->
    io:format("fun parent:m1/1 ,and parameter is ~p~n",[Param])
        .

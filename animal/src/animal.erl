-module(animal,[Name]).
-export([say/0]).


say()->
    io:format("hello,I am ~p~n",[Name])
    .

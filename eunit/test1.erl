-module(test1).
-export([hello_test/0,add/2]).

%% fun end with _test  视为test
hello_test()->
    2=add(1,1),
    3=add(1,1)
        .

add(A,B)->
    A+B.


%% eunit:test(test1).

-module(test3).
-include_lib("eunit/include/eunit.hrl").
%% 断言

hello_test()->
    ?assertEqual(2,add(1,1)),
    %% ?assert(Expression), ?assertNot(Expression)
    %% ?assertMatch(Pattern, Expression)
    ?assert(is_number(add(1,2)))
     ?assertEqual(3,add(1,1))
        .

hello2_test()->
    %% ?assertError(Pattern, Expression)
    ?assertError(badarith, 1/0)
    %% ?assertThrow(Pattern, Expression)
    %% ?assertExit(Pattern, Expression)
    %% ?assertException(Class, Pattern, Expression)
        .

add(A,B)->
    A+B.


%% eunit:test(test1).

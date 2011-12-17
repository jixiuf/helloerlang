-module(test_generators).
-include_lib("eunit/include/eunit.hrl").
-export([add/2]).


add(A,B)->
    A+B.

%% function_test() -> ?assert(A == B).
%% function_test_() -> ?_assert(A == B).
%%以?_ 开头的assert 与?assert 的不同是
%%， ?_assert(A == B) 与 fun() -> ?assert(A,B) end.等同
%%在一个test 函数里写多个?_assert 与写多个test 函数效果相同.

%% function_test_() is called a test generator function, while ?_assert(A == B)
%% is called a test generator.
%% It is called that way, because secretly, the
%% underlying implementation of ?_assert(A == B) is fun() -> ?assert(A,B) end.
%% That is to say, a function that generates a test.

%% 寻找  _test_() 结尾的方法作为test
add_test_() ->
    %% 将结果进行组合,可以嵌套
    %% 如test_them_types()与test_them_values() 的返回值是单个或多个?_assert
    [test_them_types(),
     test_them_values(),
     ?_assertError(badarith, 1/0)].

%% underlying implementation of
%% ?_assert(A == B) is
%% fun() -> ?assert(A,B) end.
%%test_them_types并不以_test 或_test_ 结尾，不会视为test,
%% 它只在add_test_()被调用.
test_them_types() ->
    ?_assert(is_number(test_generators:add(1,2))).

test_them_values() ->
    [?_assertEqual(4, test_generators:add(2,2)),
     ?_assertEqual(3, test_generators:add(1,2)),
     ?_assertEqual(3, test_generators:add(1,1))].

%% eunit:test(test_generators).

%% 也可以只测试某个 *_test_(),而非全部
%% {generator, Fun}
%% eunit:test({generator, fun test_generators:add_test_/0}).


%% {module, Mod} runs all tests in Mod
%% {dir, Path} runs all the tests for the modules found in Path
%% {file, Path} runs all the tests found in a single compiled module
%% {generator, Fun} runs a single generator function as a test, as seen above
%% {application, AppName} runs all the tests for all the modules mentioned

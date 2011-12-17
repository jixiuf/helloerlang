-module(add_tests).
-include_lib("eunit/include/eunit.hrl").

add1_test()->
    2=add:add1(1,1)
        .
%% src 与test 分开
%% 比如 src 的module 名为 add.erl ,
%% 则相应的test 模块为add_tests.erl
%% 并且在此模块中加入include
%% -include_lib("eunit/include/eunit.hrl").

%% ;; 此处是
%% eunit:test(add).
%% ;; 而非eunit:test(add_tests).

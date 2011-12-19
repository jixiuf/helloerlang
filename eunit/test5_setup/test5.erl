-module(test5).
-include_lib("eunit/include/eunit.hrl").

%% {setup, Setup, Instantiator}
%% {setup, Setup, Cleanup, Instantiator}
%% {setup, Where, Setup, Instantiator}
%% {setup, Where, Setup, Cleanup, Instantiator}

%%关于Setup Cleanup 与junit 类似
%% Setup fun/0 ,
%% Cleanup 以Setup 的返回值作参数,进行清理工作
%% Instantiator以Setup 的返回值作参数,并且返回a test set(可以是一系列 ?_assert断言)
%% Where    Specifies how to run the tests: local, spawn, {spawn, node()}.

start() ->
    InitV=1,
    InitV.

stop(InitV) ->                                  %参数从setup/1取得，
    io:format("stop ,initvalue is:~p ~n",[InitV])
    .

real_test_fun(InitV) ->
    io:format("get param from fun/1 setup:~p ~n",[InitV]),
    %% register 两次，产生一个error ，以便 测试
    [?_assertEqual(1, 0)].

a_test_() ->
    {setup,
     fun start/0,               % setup function
     fun stop/1,                % teardown function
     fun real_test_fun/1}.  % instantiator

%% some_test_() ->
%% [{setup, fun start/0, fun stop/1, fun some_instantiator1/1},
%% {setup, fun start/0, fun stop/1, fun some_instantiator2/1},
%% {setup, fun start/0, fun stop/1, fun some_instantiatorN/1}].
%% foreach 与setup 的不同是，Instantiator是一个list ,Instantiator函数
%% 只是为避麻烦
%% {foreach, Where, Setup, Cleanup, [Instantiator]}
%% {foreach, Setup, Cleanup, [Instantiator]}
%% {foreach, Where, Setup, [Instantiator]}
%% {foreach, Setup, [Instantiator]}


%% eunit:test(test).

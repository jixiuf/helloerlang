%% -*- coding:utf-8 -*-
-module(function_spce).
%% 函数的规范定义
%% -spec Module:Function(ArgType1, ..., ArgTypeN) -> ReturnType.

%% 函数的参数数目必须与函数规范定义相同，否则编译出错。
%% 在同一个module内部，可以简化为:
%% -spec Function(ArgType1, ..., ArgTypeN) -> ReturnType.

%% 同时，为了便于我们生成文档，我们可以指明参数的名称:
%% -spec Function(ArgName1 :: Type1, ..., ArgNameN :: TypeN) -> RT.

%% 函数的spec声明可以重载。通过 ';' 来实现:
%% -spec foo(pos_integer()) -> pos_integer()
%%            ; (integer()) -> integer().

%% 我们可以通过spec指明函数的输入和输出的某些关系:
-spec id(X) -> X.
id(X)->
    X.


%% 但是，对于上面的spec，其对输入输出没有任何限定。我们可以对返回值增加一些类似guard的限定:
%% 其表示X为一个tuple类型。目前仅仅支持 is_subtype 是唯一支持的guard。
-spec id2(X) -> X when is_subtype(X, tuple()).
id2(X)->
    X.

%% 某些情况下，有些函数是server的主循环，或者忽略返回值，仅仅抛出某个异常，我们可以使用 no_return() 作为返回值类型:
-spec my_error(term()) -> no_return().
my_error(Err) -> erlang:throw({error, Err}).

%% -*- coding:utf-8 -*-
-module(test_type).
%% type 定义 示例
-type my_type():: integer|true|false.

%% 类型定义也可以参数化，我们可以在括号中包含类型，如同Erlang中变量定义，这个参数必须以大写字母开头，一个简单的例子:
-type orddict(Key, Val) :: [{Key, Val}].


%% 在record 中使用类型声明
%% 语法如下:
-record(rec, {field1 :: my_type(), field2, field3 :: my_type()}).

%% 如果字段没有指明类型声明，那么默认为 any() . 比如，上面的record定义与此相同:
-record(rec2, {field1 :: my_type(), field2 :: any(), field3 :: my_type()}).

%% 如果我们在定义record的时候，指明了初始值，类型声明必须位于初始值之后:
-record(rec3, {field1 = [] :: my_type(), field2, field3 = 42 :: my_type()}).

-record(rec4, {field1 = [] :: my_type(), field2, field3 = 42 :: integer()|boolean()}).



%% 如果初始值类型与字段的类型声明不一致，会产生一个编译期错误。 filed的默认值为
%% 'undefined' ，因此下面的来个record定义效果相同:

-record(rec5, {f1 = 42  :: integer(),
                f2      :: float(),
               f3       :: 'a'|'b'}).

-record(rec6, {f1 = 42  :: integer(),
                f2      :: 'undefined' | float(),
                f3      :: 'undefined' | 'a' | 'b'}).

%% 所以，推荐您在定义record时，指明初始值。


%% record定义后，我们可以作为一个类型来使用，其用法如下:
%% #rec5{}
-record(cron_field, {
        type  :: my_type(),   % field type
        value :: my_type()              % field value
    }).
-type cron_field() :: #cron_field{}.

%% 在使用recored类型时，我们也可以重新指定某个field的类型:
%% 没有指明的filed，类型与record定义时指明的类型相同。
%% #rec6{some_field :: my_type()}.

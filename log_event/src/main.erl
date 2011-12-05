-module(main).
-compile([export_all]).

start()->
    gen_event:start_link({local,logger}),       %本地register 为logger
    gen_event:add_handler(logger,console_handle,[]),
    gen_event:add_handler(logger,file_handle,["logger.out"])
    .
test()->
    gen_event:notify(logger,hello),
    gen_event:delete_handler(logger,console_handle,[]), %第三个参数[] 传给 console_handle ：terminal() 第一个参数
    gen_event:notify(logger,hello),                     %hello为msg
    gen_event:stop(logger)                              %一般由supor 关闭，不会也可自行关闭
        .
%% make:all([load]).
%% main:start().
%% main:test().

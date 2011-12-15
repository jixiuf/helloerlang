-module(erlcount_lib_test).
-compile([export_all]).

test()->
    case erlcount_lib:find_dir("d:/tmp/helloerlang/") of
        {continue,File,F}->
            io:format("~p",[File]),
            F() ;
        done ->
            io:format("done~n",[])
    end
        .

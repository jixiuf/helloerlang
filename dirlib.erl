-module(dirlib).
-include_lib("kernel/include/file.hrl").
 -export([find_dir/1]).
%% 遍历目录
%%eq:
%% case dirlib:find_dir(Dir)  of
%%     {continue,File,ContinuationFun }->
%%         io:format("~p~n",[File]) ,
%%         ContinuationFun()        ;
%%     done ->
%%         done
%%     end


%% 返回值有二:
%%{continue,File,fun() -> dequeue_and_run(Queue)end }
%% done
find_dir(Directory)->
    find_dir(Directory,queue:new())
        .
%% private method  , use find_dir/1 instead.
find_dir(Directory,Queue)->
    %% io:format("find_dir/2 is called for ~p~n",[Directory]),
    {ok,F=#file_info{}}=file:read_file_info(Directory),
    case F#file_info.type of
        directory->
            handle_dir(Directory,Queue);
        regular ->
            handle_regular_file(Directory,Queue) ;
        _Other ->
            dequeue_and_run(Queue)
    end
        .
%% open direcotory and enqueue(入队) files in there
handle_dir(Directory,Queue)->
    %% io:format("handle directory: ~p~n",[Directory]),
    case file:list_dir(Directory) of
        {ok,[]}->
            dequeue_and_run(Queue);             %若当前目录为空目录 ，无子可入，继续从队列里取出一个进行处理
        {ok,Files} ->                           %若当前目录 非空，将子加入队列
            dequeue_and_run(enqueue_many(Directory,Files,Queue))
    end
        .

%% 将所有Files 入队
enqueue_many(Directory,FileNames,Queue)->
    %% io:format("enqueue many files in ~p...~n",[Directory]),
    F = fun (FileName,Q)->
                FullPath=filename:join(Directory,FileName),
                %% io:format("add ~p in ~n",[FullPath]) ,
                queue:in(FullPath,Q)
        end ,
    lists:foldl(F,Queue,FileNames)
        .

handle_regular_file(File,Queue)->
    %% io:format("handle regular file :~p~n",[File]),
    case  filename:extension(File) of
        ".erl"->
            {continue,File,fun() -> dequeue_and_run(Queue)end };
        _OtherExt ->
            dequeue_and_run(Queue)
    end

        .
%% pop an item and run it
dequeue_and_run(Queue)->
    %% io:format("dequeue and running...~n",[]),
    case queue:out(Queue) of
        {empty,_}->
            done;
        {{value,File},NewQueue} ->
            %% io:format("pop item ~p~n",[File]),
            find_dir(File,NewQueue)             %递归调用
    end

        .

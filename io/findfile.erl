-module(findfile).
-export([test/0]).
-export([with_file_in_dir/3]).

-include_lib("kernel/include/file.hrl").
%% Dir is a directory name ,
%% Pattern is a regexp pattern ,it is use
%% to filter Files ,only file match Pattern will be used
%% and Fun is a function accept one parameter
%% the paramter is File found under directory Dir

with_file_in_dir(Dir,Pattern,Fun)->
    case file:list_dir(Dir)  of
        {ok,Files}->
            with_file_in_dir_recurively(Dir,Files,Pattern,Fun,[]);
        {error, Reason} ->
            io:format("error~p~n",[Reason]),
            {error,{Dir,Reason}}
    end

        .

with_file_in_dir_recurively(Dir,[HeadFile|OtherFiles],Pattern,Fun,Result)->
    FullPathFile = filename:join([Dir++"/"++HeadFile]),
    case file:read_file_info(FullPathFile) of
        {ok,FileInfo} when FileInfo#file_info.type == directory->
            case with_file_in_dir(FullPathFile,Pattern,Fun) of
                {error,Reason}->
                    {error,{Dir,Reason}} ;
                List->
                    with_file_in_dir_recurively(Dir,OtherFiles,Pattern,Fun,Result++List)
            end

                ;
        {ok,FileInfo} when FileInfo#file_info.type == regular->
            try re:run(HeadFile,Pattern)   of
                {match,_Result}->
                    with_file_in_dir_recurively(Dir,OtherFiles,Pattern,Fun,[Fun(FullPathFile)|Result]);
                nomatch->
                    with_file_in_dir_recurively(Dir,OtherFiles,Pattern,Fun,Result)
            catch _->
                    {error,{regexp_pattern_error,FileInfo}}
            end

                ;
        {error,Reason}->
            {error,{Dir,Reason}}
    end
        ;
with_file_in_dir_recurively(_Dir,[],_Pattern,_Fun,Result) ->
    Result
    .

test()->
    with_file_in_dir("d:/tmp/helloerlang" ,".*.erl$" ,fun(File)->  io:format("~p~n",[File]) end )
        .

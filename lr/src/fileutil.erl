-module(fileutil).
-export([get_file_name_prefix/1,find_next_log_file/1]).
-compile([export_all]).

-include_lib("kernel/include/file.hrl").

-spec find_next_log_file(FileName) -> undefined when
      FileName::string()|undefined.

get_file_name_prefix(FileName)->
    case get_log_filename_token(FileName) of
        {Prefix,_TimeStamp}->
            Prefix;
        undefined ->
            undefined
    end

        .
find_next_log_file(undefined)->
    find_the_nearest_log_file();
find_next_log_file(CurrentLogFile)when is_list(CurrentLogFile)->
    case get_log_filename_token(CurrentLogFile) of
        {_Prefix,TimeStamp}->
            find_the_nearest_log_file(TimeStamp);
        undefined ->
            find_the_nearest_log_file()
    end
        .

find_the_nearest_log_file()->
    find_the_nearest_log_file(0)
    .
find_the_nearest_log_file(TimeStamp)->
    {ok,Dir}=file:list_dir(log_reader:get_current_app_env(logdir)),
    find_the_nearest_log_file(Dir,TimeStamp,[])
        .

find_the_nearest_log_file([],_TimeStamp,[])->
    undefined;
find_the_nearest_log_file([],_TimeStamp,[{_TimeStamp2,FileName}])->
    FileName;
find_the_nearest_log_file([H|Files],TimeStamp,[])->
    case get_log_filename_token(H) of
        {_Prefix,TimeStamp2} when TimeStamp2>TimeStamp ->
            find_the_nearest_log_file(Files,TimeStamp,[{TimeStamp2,H}]);
        _ ->
            find_the_nearest_log_file(Files,TimeStamp,[])
    end;
find_the_nearest_log_file([H|Files],TimeStamp,[{TimeStamp2,_File}])->
    case get_log_filename_token(H) of
        {_Prefix,TimeStamp3} when TimeStamp3>TimeStamp,TimeStamp3<TimeStamp2 ->
            find_the_nearest_log_file(Files,TimeStamp,[{TimeStamp3,H}]);
        _ ->
            find_the_nearest_log_file(Files,TimeStamp,[{TimeStamp2,_File}])
    end

        .


get_log_filename_token(LogFileName)->
    case catch re:run(LogFileName,log_reader:get_current_app_env(log_file_pattern),[{capture,[1,2]}]) of
        {match,[{PrefixIndex,PredixLength},{TimeStampIndex,TimeStampLength}]}->
            Prefix= string:substr(LogFileName,PrefixIndex+1,PredixLength),
            TimeStamp=list_to_integer(string:substr(LogFileName,TimeStampIndex+1,TimeStampLength)),
            {Prefix,TimeStamp};
        _->
            undefined
    end

        .

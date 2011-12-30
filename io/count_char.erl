-module(count_char).
-export([file/0]).
-include_lib("eunit/include/eunit.hrl").

file()->
    io:format("count char in a file .~n",[]),
    case file:open("count_char.erl",[read,raw,binary]) of
        {ok,Fh}->
            scan_file(Fh,0,file:read(Fh,1024));
        {error,Reason}->
            io:format("fail to open file with reason:~p .~n",[Reason])
    end
        .
%% return the size of a file
%% return {ok,Count} ,or {error,Reason}
scan_file(FileHandle,Count,{ok,Binnary})->
    scan_file(FileHandle,Count+count_x(Binnary),file:read(FileHandle,1024));
scan_file(FileHandle,Count,eof) ->
    file:close(FileHandle),
    {ok,Count};
scan_file(FileHandle,_Count,{error,Reason}) ->
    file:close(FileHandle),
    {error,Reason}
        .
%%
%% return the size of a binnary data
count_x(Binnary)->
    count_x(binary_to_list(Binnary),0)
        .
count_x([],Count)->
    Count;
count_x([_|Tail],Count) ->
    count_x(Tail,Count+1).


count_x_test()->
    ?assertEqual(count_x(<<"abc">>),3)
        .
%% eunit:test([count_char]).
%% count_char:file()

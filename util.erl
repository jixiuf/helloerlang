-module(util).
-export([string_2_datetime/1,string_2_date/1,time_to_string/1]).

%% Time (as return by calendar:local_time() to string conversion.
%% "2012-01-06 09:55:12"
time_to_string( {{Y,Mo,D},{H,Mi,S}} ) ->
    String = io_lib:format( "~4.4w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                            [Y,Mo,D,H,Mi,S] ),

         lists:flatten(String).
%% 好像不太对。仅到秒
%% timestamp_to_datetime(TimeStamp)->
%%     Second1 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
%%     DateTime = TimeStamp +Second1,
%%     calendar:datetime_to_gregorian_seconds(DateTime)
%%     .

%% datetime_to_timestamp({Date,Time})->
%%     Second1 = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
%%     Second2=calendar:datetime_to_gregorian_seconds({Date,Time}),
%%     Second2-Second1
%%
%%string_2_date("2011-1-1")
string_2_date(DateStr)->
    list_to_tuple( lists:map(fun erlang:list_to_integer/1, string:tokens(DateStr,"-"))).

%% util:string_2_datetime("2011-1-1 10:10:10").
string_2_datetime(DateTimeStr)->
    [DateStr,TimeStr]= string:tokens(DateTimeStr," "),
    Date=list_to_tuple( lists:map(fun erlang:list_to_integer/1, string:tokens(DateStr,"-"))),
    Time=list_to_tuple( lists:map(fun erlang:list_to_integer/1, string:tokens(TimeStr,":"))),
    {Date,Time}
        .

-module(util).
-export([get_localips/0,get_localip/0,string_2_datetime/1,string_2_date/1,time_to_string/1]).

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

get_localips()->
	case inet:getif() of
		{ok,IFs}->
			SortedIFs = lists:sort(fun(IP1,IP2)->
										   {{I1,I2,I3,I4},_,_} = IP1,
										   {{J1,J2,J3,J4},_,_} = IP2,
										   if I1 =:= 192 -> true;
											  J1 =:= 192 -> false;
											  I1 =:= 127 -> true;
											  J1 =:= 127 -> false;
											  I1 < J1 -> true;
											  I1 > J1 -> false;
											  I2 < J2 -> true;
											  I2 > J2 -> false;
											  I3 < J3 -> true;
											  I3 > J3 -> false;
											  I4 < J4 -> true;
											  I4 > J4 -> false;
											  true-> false
										   end
								   end, IFs),
			lists:map(fun(IfConfig)->
							case IfConfig of
								{{192,168,I3,I4},_,_}-> "192.168." ++ integer_to_list(I3) ++"." ++ integer_to_list(I4);
								{{127,0,0,I4},_,_}->"127.0.0." ++ integer_to_list(I4);
								{{10,I2,I3,I4},_,_}->str_util:sprintf("10.~p.~p.~p", [I2,I3,I4]);
								{{I1,I2,I3,I4},_,_}->str_util:sprintf("~p.~p.~p.~p", [I1,I2,I3,I4])
							end
					end,SortedIFs);
		_->[]
	end.
get_localip()->
	case get_localips() of
		[]->[];
		[IP|_]-> IP
	end.

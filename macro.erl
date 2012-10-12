%%%-------------------------------------------------------------------
%%% @author 纪秀峰 <jixiuf@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 2012-10-10 15:47 by 纪秀峰 <jixiuf@gmail.com>
%%  Last Updated: 纪秀峰 2012-10-12 19:36:46 星期五
%%%-------------------------------------------------------------------
-module(macro).
-export([test/0]).
-define(VERYFY(Value,ErrorId),case Value of false -> throw(ErrorId);true-> ok end) .
-define(CASE(TEST,TRUE,FALSE),case TEST of true->TRUE; false ->FALSE end ).

-define(FE(List__,E,Eval__),
        lists:foreach(
          fun(E)->
                  Eval__
          end,List__
         )).

%% -define(FOLD(List__,Init__,Eval__),
%%         lists:foldl(
%%           fun(E,Acc)->
%%                   Eval__
%%           end,Init__,List__
%%          )).


-define(FOLD(List__,Init__,E,Acc,Eval__),
        lists:foldl(
          fun(E,Acc)->
                  Eval__
          end,Init__,List__
         )).

-define(TRY(Eval__),
        try
            Eval__
        catch
            throw:{errorid ,ErrorId}->
                io:format("handle errorid:~ts~n",[ErrorId]);
                %% ResultMsg =pill_packet:encode_pill_error_s2c(ErrorId),
                %% role_op:send_data_to_gate(ResultMsg);
            _:Msg->
                slogger:msg("~p~n",[Msg]),
                ok
        end
       ).

test()->
    %% io:format("~p~n",[?CASE(1>2 ,1,2)])
    ?FE([[1,11],[2,22],[3,33]],E,
        begin
            ?FE(E,E2,
                begin
                    io:format("~p,",[E2])
                end
               ),
            io:format("~n",[])

        end),
    Sum=?FOLD([1,2,3],0,E,Acc,
              begin
                  E+Acc
              end),
    io:format("sum=~p~n",[Sum]),
    ?TRY(begin
             ?VERYFY(1==2,{errorid,"1!=2"})
         end)
        .

-module(log_reader).
-export([start/2,stop/1,get_current_app_env/1,get_current_app_env/2]).

-spec get_current_app_env(Var) -> 'undefined' | {'ok', Val} when
      Var :: atom(),
      Val :: term().
get_current_app_env(Var)->
    case application:get_env(?MODULE,Var) of
        {ok,Val}->
            Val;
        _ ->
            undefined
    end.


-spec get_current_app_env(Var,DefaultVal) -> 'undefined' | {'ok', Val} when
      Var :: atom(),
      DefaultVal :: term(),
      Val :: term().
get_current_app_env(Var,DefaultVal)->
    case application:get_env(Var) of
        undefined->
            DefaultVal;
        {ok,Val}->
            Val
    end.



start(_Normal,_Args)->
    {ok ,spawn(fun()-> receive
                           _->
                               ok
                       end
               end
              )}    .

stop(_State)->
    ok.

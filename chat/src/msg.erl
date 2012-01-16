-module(msg).
-export([get/1]).


get("echo")->
    <<1:32>>;
get(_Msg) ->
    <<0:32>>
    .

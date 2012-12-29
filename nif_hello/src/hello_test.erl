%%%-------------------------------------------------------------------
%%% @author 纪秀峰 <jixiuf@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 2012-12-29 10:18 by 纪秀峰 <jixiuf@gmail.com>
%%  Last Updated: 纪秀峰 2012-12-29 10:30:38 星期六
%%%-------------------------------------------------------------------
-module(hello_test).
-export([hello/0,init/0]).

-define(VERSION,0).

init()->
    erlang:load_nif("./hello_c",?VERSION).
hello()->
    "NIF function not loaded ."
    .

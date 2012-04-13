-module(game_log_server).

-export([get_current_app_env/2,get_current_app_env/1,start/2,stop/1]).
-export([get_random_node/1,send_log/2,get_all_mysql_proxy_node/0,get_game_log_server_listener/0]).

start(normal,_Args)->
    io:format("game_log_server start/2 is running...~n",[]),
    emysql_center_sup:start_link();
start({failover,Node},_Args)->
    io:format("game_log_server{failover from ~p} start/2 is running...~n",[Node]),
    emysql_center_sup:start_link();
start({takeover,Node},_Args)->
    io:format("game_log_server{takeover from ~p} start/2 is running...~n",[Node]),
    emysql_center_sup:start_link().

stop(State)->
    io:format("game_log_server is stopped. with state:~p~n",[State]),
    emysql_center_sup:stop(),
    ok.

-spec get_current_app_env(Var) -> 'undefined' | {'ok', Val} when
   Var :: atom(),
   Val :: term().
get_current_app_env(Var)->
    application:get_env(?MODULE,Var).

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

%% 返回global注册名为 game_log_server的pid 或者 undefined
%%不要直接调用  global:whereis_name(game_log_server)
%%
-spec get_game_log_server_listener() -> 'undefined' |pid().
get_game_log_server_listener()->
    emysql_center:get_game_log_server_listener().
-spec get_all_mysql_proxy_node()-> 'undefined'| [node()].
get_all_mysql_proxy_node()->
    emysql_center:get_all_mysql_proxy_node().

%%-----------------------------------------------------------------------------
%% @doc
%% 记录一条日志信息，成功返回ok, 失败返回error
%% @spec send_log(Node::node(), #tbl_onoff{}=Record::any()) ->
%%       ok|error
%% @end
%%-----------------------------------------------------------------------------
-spec send_log(node(),any())-> ok|error.
send_log(RandomMySqlProxyNode,Record) ->
    emysql_center:send_log(RandomMySqlProxyNode,Record).

%% 从众多的mysql 列表中随机获得一个
%% 此方法 在客户端执行,未访问服务器，
get_random_node(NodeList)->
    lists:nth(random:uniform(length(NodeList)),NodeList).

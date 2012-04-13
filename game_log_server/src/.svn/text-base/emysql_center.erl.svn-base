-module(emysql_center).
-behaviour(gen_server).

-export([stop/0,start_link/0]).
%%对外接口
-export([send_log/2,add_node/1,get_all_mysql_proxy_node/0,get_random_mysql_proxy_node/1,get_all_mysql_proxy_node/1]).
-export([quote/1]).
%% gen_server export
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

-include("tbl_fileds.hrl").
-include_lib("../deps/emysql/include/emysql.hrl").

%%有问题，等将来数据写入较多的时候，这个服务会处理不过来，我们刚改了这样一个类似的瓶颈，发现进程处理消息的能力大概在1秒1k条左右
%% 1 开多个数据库代理服务
%% 2 让进程自己直接读写

%% 以上的处理会带来另外一个瓶颈，就是数据库的读写能力，如果可能，最后做一个数据持久服务
%% 呃，叫法不同，也有叫档案容器、数据服务什么的，主要是建立内存数据库，平稳数据库读写的东东
%% 带些缓存的。mysql 有 memcached的缓存插件。
%% 数据库没有分布式。。数据量大么  大的话会有瓶颈
%% 单机mysql并发撑不住多少。除非极度优化
%% 一般的读写分离，也解决不了并发高问题
%% 你相当于多个分布式节点  向一个数据库里写入   节点是分布式了  数据库可是只有一个的
%% 以前我们解决的是 redis+oracle。 这样就解决IO问题，批量写入

-define(DEF_NODE_LIST, [{'192.168.0.44',3}]).
-define(DEF_CONNECTION_COUNT_EACH_NODE,3).
-define(DEF_MYSQL_USERNAME,"tyLog").
-define(DEF_MYSQL_PASSWORD,"2104").
-define(DEF_MYSQL_HOSTNAME,"192.168.0.200").
-define(DEF_MYSQL_PORT,3306).
-define(DEF_MYSQL_DBNAME,"tyLog").
-define(DEF_MYSQL_ENCODING,utf8).
-define(DEF_POOL_ID,hello_pool).
-define(LISTENER,game_log_server).
-record(state,{node_list,node_count,node_pid_set}).


%%-----------------------------------------------------------------------------
%% @doc
%% 记录一条日志信息，成功返回ok, 失败返回error
%% @spec send_log(Node::node(), #tbl_onoff{}=Record::any()) ->
%%       ok|error
%% @end
%%-----------------------------------------------------------------------------
-spec send_log(node(),any())-> ok|error.
send_log(RandomMySqlProxyNode,#tbl_onoff{}=Record) ->
    execute(RandomMySqlProxyNode,{addOnOff,Record}).

-spec execute(node(),fun()) -> any().
execute(RandomMySqlProxyNode,CommandMsg) ->
    io:format("~p~n",[RandomMySqlProxyNode]),
    rpc:call(RandomMySqlProxyNode, emysql_execute, execute, [CommandMsg]).

%% get_random_mysql_proxy_node()->
%%     gen_server:call(?LISTENER,get_random_mysql_proxy_node).

%%-----------------------------------------------------------------------------
%% @doc
%%  get a random node ,for executing mysql query .
%% the Pid or global registered name is ?LISTENER
%% @spec get_random_mysql_proxy_node(Pid::pid()) ->
%%       node()
%% @end
%%-----------------------------------------------------------------------------
-spec get_random_mysql_proxy_node(pid()) -> node().
get_random_mysql_proxy_node(Pid) when is_pid(Pid)->
    gen_server:call(Pid,get_random_mysql_proxy_node);
get_random_mysql_proxy_node(Global_name) when is_atom(Global_name)->
    gen_server:call( global:whereis_name(Global_name) ,get_random_mysql_proxy_node) .

get_all_mysql_proxy_node()->
    gen_server:call( global:whereis_name(?LISTENER),get_all_mysql_proxy_node).

get_all_mysql_proxy_node(Pid)when is_pid(Pid)->
    gen_server:call(Pid,get_all_mysql_proxy_node);
get_all_mysql_proxy_node(Global_name)when is_atom(Global_name) ->
    gen_server:call( global:whereis_name(Global_name) ,get_all_mysql_proxy_node).
%% = get_random_mysql_proxy_node(),

stop()->
    gen_server:cast( global:whereis_name(?LISTENER),stop)
    .

add_node(Node) when is_atom(Node) ->
    gen_server:cast( global:whereis_name(?LISTENER),{add_node,Node}).

start_link()->
    NodeListInfo = game_log_server:get_current_app_env(node_list_info,?DEF_NODE_LIST),
    io:format("~p~n",[NodeListInfo]),
    random:seed(now()),
    NodeList=lists:foldl(fun({IP,Count},Acc)->
                              NewNodeList=lists:map(fun(_)->
                                                         Name="mysql_proxy_"++integer_to_list(random:uniform(10000)),
                                                         if  is_atom(IP)
                                                             -> list_to_atom(Name ++ "@" ++ atom_to_list(IP)) ;
                                                             true ->
                                                                 list_to_atom(Name ++ "@" ++ IP)
                                                         end
                                                 end ,lists:seq(1,Count)),
                              NewNodeList++ Acc
                      end ,[],NodeListInfo),
    gen_server:start_link({global, ?LISTENER},?MODULE,#state{node_list=NodeList,
                                                             node_count=length(NodeList)},[]).

init(S)->
    NodeList = S#state.node_list,
    process_flag(trap_exit,true),
    %% (PoolId,Host,Port,User,Password,Database,LogFun,Encoding)
    lists:foreach( fun start_node/1,NodeList),
    {ok,S}.

start_node(Node)->
    io:format("starting node:~p~n",[Node]),
    case node() of
        Node->
            ok;           %if node is current node ,don't need start it as a slave node .
        _->
            case net_adm:ping(Node) of
                pong->                          %already started
                    ok;
                pang ->
                    [Name,Host] = string:tokens(atom_to_list(Node),"@"),
                    slave:start_link(Host,Name,"-rsh ssh -setcookie " ++  game_log_server:get_current_app_env(cookie,atom_to_list(erlang:get_cookie()))++ "  +Mea r10b +P 102400"),
                    monitor_node(Node, true)
            end
    end,

    rpc:call( Node,crypto,start, []),
    rpc:call( Node,application,start, [emysql]),
	%% emysql:add_pool(hello_pool, 1, "root", "root", "localhost", 3306, "test", utf8)
    rpc:call( Node,emysql ,add_pool,[game_log_server:get_current_app_env(pool_id ,?DEF_POOL_ID),
                                    game_log_server:get_current_app_env(connection_count_each_node,?DEF_CONNECTION_COUNT_EACH_NODE),
                                    game_log_server:get_current_app_env(mysql_username,?DEF_MYSQL_USERNAME),
                                    game_log_server:get_current_app_env(mysql_password,?DEF_MYSQL_PASSWORD),
                                    game_log_server:get_current_app_env(mysql_hostname,?DEF_MYSQL_HOSTNAME),
                                    game_log_server:get_current_app_env(mysql_port,?DEF_MYSQL_PORT),
                                    game_log_server:get_current_app_env(mysql_dbname,?DEF_MYSQL_DBNAME),
                                    game_log_server:get_current_app_env(mysql_encoding,?DEF_MYSQL_ENCODING)]).

handle_call(get_random_mysql_proxy_node,_From,State)->
    Re=lists:nth(random:uniform(State#state.node_count),State#state.node_list),
    {reply, Re,State};
handle_call(get_all_mysql_proxy_node,_From,State)->
    NodeList=State#state.node_list,
    {reply, NodeList,State};
handle_call(_Request,_From,State)->
    {noreply, State}.

handle_cast({add_node,Node},State)->
    NodeList=State#state.node_list,
    start_node(Node),
    {noreply,State#state{node_list=[Node|NodeList]}};
handle_cast(stop,State=#state{node_list=NodeList})->
    spawn(fun()->
                  lists:foreach(fun(Node)-> slave:stop(Node) end ,NodeList)
          end ),
    {stop,normal,State};
handle_cast(_Request,State) ->

    {noreply, State} .


handle_info({nodedown,Node},State)->
    io:format("nodedown:~p ~n",[Node]),
    %% start_node(Node),
    {noreply, State};
handle_info(Info,State)->
    io:format("~p~n",[Info]) ,
    {noreply, State}.

terminate(Reason,_State)->
    io:format("emysql_center terminate with reason:~p~n",[Reason]),
    ok .

code_change(_Previous_Version,State,_Extra)->
    io:format("~p code_change/2 is called~n",[?MODULE]) ,
    {ok,State} .
%% erl -kernel start_boot_server true boot_server_slaves '[{10,0,0,253},{10,0,0,252}]' -name master@10.0.0.253 -setcookie abc -rsh ssh
%% slave:start('10.0.0.252', slave1, "-setcookie abc -loader inet -hosts '10.0.0.253' -id master@10.0.0.253").

%% util func
quote(undefined)->
    "null";
quote(Atom) when is_atom(Atom)->
    atom_to_list(Atom);
quote(Int) when is_integer(Int)->
    integer_to_list(Int);
quote(Val)  ->
    emysql_util:quote(Val).

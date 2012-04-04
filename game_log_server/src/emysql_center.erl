-module(emysql_center).
-export([send_log/2,add_node/1,get_all_mysql_proxy_node/0,get_random_mysql_proxy_node/0,assert/0,start_link/0,get_random_mysql_proxy_node/1,get_all_mysql_proxy_node/1,execute/2]).
-behaviour(gen_server).
-include("tbl_fileds.hrl").
-include_lib("emysql/include/emysql.hrl").

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

%% 流量控制
-import(emysql_center_app, [get_current_app_env/2]).

-define(nodes,[node(),'c@jf.org','c2@jf.org','c3@jf.org']).
%% -define(nodes,[node()]).

-define(connection_count_each_node,3).
-define(cookie,"DJQWUOCYZCIZNETCXWES").
-define(mysql_username,"root").
-define(mysql_password,"root").
-define(mysql_hostname,"jf.org").
-define(mysql_port,3306).
-define(mysql_dbname,"gamelog").
-define(mysql_encoding,utf8).
-define(pool_id,hello_pool).
-define(listener,?MODULE).

-record(state,{nodes,node_count,node_pid_set}).


%% erl -rsh ssh -name clustmaster@jf.org
%% 或
%%  erl -name clustmaster@jf.org
%% slave:start_link("jf.org","c2","-rsh ssh -setcookie DJQWUOCYZCIZNETCXWES  +Mea r10b ").

%% 每个节点上开启多少个mysql连接
%% {ok,P}=?MODULE:start_link().
%% F= fun()-> ?MODULE:test() end.
%% ?MODULE:execute(P,F).

%%对外接口

%% gen_server export
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

%% record_to_sql_insert(Record)->
%%     RecordType = element(1 ,Record),
%%     Fields=record_info(fields, RecordType),
%%     lists:foldl( fun (Field ,Acc)->

%%                          end ,["insert into " RecordType " values( " ],Fields)

%%         .

quote(undefined)->
    "null";
quote(Atom) when is_atom(Atom)->
    atom_to_list(Atom);
quote(Int) when is_integer(Int)->
    integer_to_list(Int);
quote(Val)  ->
    emysql_util:quote(Val).


send_log(Node,#tbl_onoff{}=Record) ->
    try
        execute(Node,fun()->
                             Sql="call addOnOff(" ++
                                 quote(Record#tbl_onoff.onOffType            ) ++","++
                                 quote(Record#tbl_onoff.accountId            ) ++","++
                                 quote(Record#tbl_onoff.accountType          ) ++","++
                                 quote(Record#tbl_onoff.playerId             ) ++","++
                                 quote(Record#tbl_onoff.playerLevel          ) ++","++
                                 quote(Record#tbl_onoff.playerName           ) ++","++
                                 quote(Record#tbl_onoff.datetime             ) ++","++
                                 quote(Record#tbl_onoff.clientVersion        ) ++","++
                                 quote(Record#tbl_onoff.clientType           ) ++","++
                                 quote(Record#tbl_onoff.issuers              ) ++","++
                                 quote(Record#tbl_onoff.flashPlayerVersion   ) ++","++
                                 quote(Record#tbl_onoff.connectType          ) ++","++
                                 quote(Record#tbl_onoff.gameServerName       ) ++
                                 ")",
                             Result=emysql:execute(hello_pool, list_to_binary(Sql)),
                             case Result of
                                 #error_packet{}->
                                     error;
                                 #ok_packet{}->
                                     ok
                             end
                     end)
    catch
        error:_Error->
            error
    end
        .

get_random_mysql_proxy_node()->
    gen_server:call(?listener,get_random_mysql_proxy_node)
        .

%%-----------------------------------------------------------------------------
%% @doc
%%  get a random node ,for executing mysql query .
%% @spec get_random_mysql_proxy_node(Pid::pid()) ->
%%       node()
%% @end
%%-----------------------------------------------------------------------------
-spec get_random_mysql_proxy_node(pid()) -> node().
get_random_mysql_proxy_node(Pid) when is_pid(Pid)->
    gen_server:call(Pid,get_random_mysql_proxy_node);
get_random_mysql_proxy_node(Global_name) when is_atom(Global_name)->
    gen_server:call( global:whereis_name(Global_name) ,get_random_mysql_proxy_node)
        .

get_all_mysql_proxy_node()->
    gen_server:call( global:whereis_name(?listener),get_all_mysql_proxy_node).

get_all_mysql_proxy_node(Pid)when is_pid(Pid)->
    gen_server:call(Pid,get_all_mysql_proxy_node);
get_all_mysql_proxy_node(Global_name)when is_atom(Global_name) ->
    gen_server:call( global:whereis_name(Global_name) ,get_all_mysql_proxy_node)
        .
%% = get_random_mysql_proxy_node(),
-spec execute(node(),fun()) -> any().
execute(RandomMySqlProxyNode,Fun) when  is_function(Fun)->
    io:format("~p~n",[RandomMySqlProxyNode]),
    rpc:call(RandomMySqlProxyNode,emysql_execute,execute,[Fun])
        .
add_node(Node) when is_atom(Node) ->
    rpc:call( global:whereis_name(?listener),{add_Node,Node})
    .
%%make sure ?MODULE:start_link() is called ,and is registered as '?MODULE'
assert()->
    case global:whereis_name(?listener) of
        undefined->
            ?MODULE:start_link();
        _P->
            ok
    end
        .
%%% gen_server 接口

start_link()->
    gen_server:start_link({global, ?listener},?MODULE,#state{nodes=get_current_app_env(nodes,?nodes),
                                                             node_count=length(get_current_app_env(nodes,?nodes))},[])
        .
init(S)->
    Nodes = S#state.nodes,
    %% (PoolId,Host,Port,User,Password,Database,LogFun,Encoding)
    lists:foreach( fun start_node/1,Nodes),
    {ok,S}
        .

start_node(Node)->
    case node() of
        Node->
            ok;           %if node is current node ,don't need start it as a slave node .
        _->
            [Name,Host] = string:tokens(atom_to_list(Node),"@"),
            slave:start_link(Host,Name,"-rsh ssh -setcookie " ++  get_current_app_env(cookie,?cookie)++ "  +Mea r10b +P 102400"),
            monitor_node(Node, true)
    end,

    rpc:call( Node,crypto,start, []),
    rpc:call( Node,application,start, [emysql]),
    rpc:call(Node,emysql ,add_pool,[get_current_app_env(pool_id ,?pool_id),
                                    get_current_app_env(connection_count_each_node,?connection_count_each_node),
                                    get_current_app_env(mysql_username,?mysql_username),
                                    get_current_app_env(mysql_password,?mysql_password),
                                    get_current_app_env(mysql_hostname,?mysql_hostname),
                                    get_current_app_env(mysql_port,?mysql_port),
                                    get_current_app_env(mysql_dbname,?mysql_dbname),
                                    get_current_app_env(mysql_encoding,?mysql_encoding)])
    %% emysql:add_pool(hello_pool, 1, "root", "root", "localhost", 3306, "test", utf8),
        .

handle_call(get_random_mysql_proxy_node,_From,State)->
    Re=lists:nth(random:uniform(State#state.node_count),State#state.nodes),
    {reply, Re,State};
handle_call(get_all_mysql_proxy_node,_From,State)->
    Nodes=State#state.nodes,
    {reply, Nodes,State};
handle_call({add_node,Node},_From,State)->
    Nodes=State#state.nodes,
    start_node(Node),
    {reply, Nodes,State#state{nodes=[Node|Nodes]}};
handle_call(_Request,_From,State)->
    {noreply, State}
        .

handle_cast(_Request,State)->
    {noreply, State} .


handle_info({nodedown,Node},State)->
    io:format("nodedown:~p ~n",[Node]),
    start_node(Node),
    {noreply, State}
        ;
handle_info(Info,State)->
    io:format("~p~n",[Info]) ,
    {noreply, State}.

terminate(_Reason,_State)->
    ok .

code_change(_Previous_Version,State,_Extra)->
    io:format("~p code_change/2 is called~n",[?MODULE]) ,
    {ok,State} .
    %% erl -kernel start_boot_server true boot_server_slaves '[{10,0,0,253},{10,0,0,252}]' -name master@10.0.0.253 -setcookie abc -rsh ssh
    %% slave:start('10.0.0.252', slave1, "-setcookie abc -loader inet -hosts '10.0.0.253' -id master@10.0.0.253").

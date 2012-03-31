-module(emysql_node).

-define(nodes,[node(),'c@jf.org','c2@jf.org','c3@jf.org']).
-define(connection_count_each_node,3).
-define(cookie,"DJQWUOCYZCIZNETCXWES").
-define(mysql_username,"root").
-define(mysql_password,"root").
-define(mysql_hostname,"jf.org").
-define(mysql_port,3306).
-define(mysql_dbname,"test").
-define(mysql_encoding,utf8).
-define(pool_id,hello_pool).

-record(state,{nodes,node_count,node_pid_set}).


%% erl -rsh ssh -name clustmaster@jf.org
%% 或
%%  erl -name clustmaster@jf.org
%% slave:start_link("jf.org","c2","-rsh ssh -setcookie DJQWUOCYZCIZNETCXWES  +Mea r10b ").

%% 每个节点上开启多少个mysql连接
%% {ok,P}=emysql_node:start_link().
%% F= fun()-> emysql_node:test() end.
%% emysql_node:execute(P,F).

%%对外接口
-export([assert/0,execute/2,start_link/0]).

%% gen_server export
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).


start_link()->
    gen_server:start_link(?MODULE,#state{nodes=?nodes,node_count=length(?nodes)},[])
        .
%%make sure emysql_node:start_link() is called ,and is registered as 'emysql_node'
assert()->
    case whereis(emysql_node) of
        undefined->
            {ok,P}=emysql_node:start_link(),
            register(emysql_node,P);
        _P->
            ok
    end
    .
execute(Pid,Fun) when is_pid(Pid)->
    gen_server:call(Pid,{execute,Fun});
execute(RegisteredName,Fun) when is_atom(RegisteredName)->
    gen_server:call(whereis(RegisteredName),{execute,Fun})
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
            slave:start_link(Host,Name,"-rsh ssh -setcookie " ++  ?cookie++ "  +Mea r10b "),
            monitor_node(Node, true)
    end,

    rpc:call( Node,crypto,start, []),
    rpc:call( Node,application,start, [emysql]),
    rpc:call(Node,emysql ,add_pool,[?pool_id, 1, ?mysql_username,?mysql_password,?mysql_hostname,?mysql_port,?mysql_dbname,?mysql_encoding])
    %% emysql:add_pool(hello_pool, 1, "root", "root", "localhost", 3306, "test", utf8),
        .

handle_call({execute,Fun},_From,State)->
    Nodes=State#state.nodes,
    NodeCount=State#state.node_count,
    RandomNode=lists:nth(random:uniform(NodeCount),Nodes),
    io:format("~p~n",[   RandomNode ]),
    Re=rpc:call(RandomNode,emysql_execute,execute,[Fun]),
    {reply, Re,State};
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
    {ok,State} .

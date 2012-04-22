-module(press).
-export([get_timestamp/0,start/2,run/3]).

-include("tbl_fileds.hrl").
-record(state,{running=0,start_time,process_count_all,sql_count_each_process}).

start(ProcessCount,SqlCountEachProcess)->
    net_kernel:connect_node('emacs@192.168.1.108'),
    CurrentTime=get_timestamp(),
    spawn_link(?MODULE,run,[ProcessCount,SqlCountEachProcess,#state{start_time=CurrentTime,
                                                                    process_count_all=ProcessCount,
                                                                    sql_count_each_process=SqlCountEachProcess}])
        .


recv(#state{running=0,start_time=StartTime,
            process_count_all=ProcessCount,
            sql_count_each_process=SqlCountEachProcess
           })->
    Usedtime =get_timestamp()-StartTime,
    io:format("process_count:~p sql count each process:~p used time:~p~n",[ProcessCount,SqlCountEachProcess,Usedtime])
        ;
recv(#state{running=Running}=State)->
    receive
        done->
            recv(State#state{running=Running-1})
    end
        .

run(0,_SqlCountEachProcess,#state{}=State)->
    recv(State);
run(ProcessCount,SqlCountEachProcess,#state{running=Running}=State) ->
    Parent =self(),
    spawn(fun()-> run_sql(SqlCountEachProcess,Parent)end),
    run(ProcessCount-1,SqlCountEachProcess,State#state{running=Running+1})
        .

run_sql(0,Parent)->
    Parent!done;
run_sql(SqlCountEachProcess,Parent) ->
    test2(),
    run_sql(SqlCountEachProcess-1 ,Parent)
        .


test2()->
    NodeList= game_log_server:get_all_mysql_proxy_node(),
    Node=game_log_server:get_random_node(NodeList),

    OnOffRecord= #tbl_on_off{
      onOffType=1,
      accountId=1,
      accountType=1,
      playerId=1,
      headHeroId=1,
      playerLevel=1,
      playerName="name",
      logTime="1989-01-26", % 用户获得经验的时间
      clientVersion="ver",
      clientType="t",
      issuers="dd",
      flashPlayerVersion="dd",
      connectType="con",
      gameServerName="name",
      ipAddr="localhost"%IP地址
     },

    %% Result3 = emysql:execute(test_pool,<<   "call addOnOff();"        >>),

    Result=game_log_server:send_log(Node,    OnOffRecord),
    case Result of
        error->
            io:format("~p~n",[Result]) ;
        _ ->
            ok
        end

    %% Pid=global:whereis_name(emysql_center)
        .
get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    ((Mega*1000000+Sec)*1000000+Micro)/1000.

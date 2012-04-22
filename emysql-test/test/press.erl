-module(press).
-export([get_timestamp/0,start/3,run/3]).
-include("emysql.hrl").

-record(state,{running=0,start_time,process_count_all,sql_count_each_process}).
%% 启动ProcessCount个进程，每个进程执行SqlCountEachProcess次sql 操作
start(ProcessCount,SqlCountEachProcess,MysqlConnectionCount)->
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(hello_pool, MysqlConnectionCount, "root", "root", "localhost", 3306, "mysqlslap", utf8),

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
    Result=    emysql:execute(hello_pool,<<"call insert_tbl_test(1,1,1,1,1,'name','1989-01-26','ver','t','dd','dd','con','name','localhost',1);">>),
    case Result of
        Rec when is_record(Rec ,ok_packet) ->
            ok;
        Rec when is_record(Rec ,error_packet) ->
            io:format("~p~n",[Result])
    end

    %% %% Pid=global:whereis_name(emysql_center)
        .
get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    ((Mega*1000000+Sec)*1000000+Micro)/1000.

%%  CREATE TABLE `tbl_test` (
%%   `id` int(11) NOT NULL AUTO_INCREMENT,
%%   `onOffType` int(11) DEFAULT NULL,
%%   `accountId` int(11) DEFAULT NULL,
%%   `accountType` int(11) DEFAULT NULL,
%%   `playerId` int(11) DEFAULT NULL,
%%   `headHeroId` int(11) DEFAULT NULL,
%%   `playerLevel` int(11) DEFAULT NULL,
%%   `playerName` varchar(255) DEFAULT NULL,
%%   `logTime` datetime DEFAULT NULL,
%%   `clientVersion` varchar(255) DEFAULT NULL,
%%   `clientType` varchar(255) DEFAULT NULL,
%%   `issuers` varchar(255) DEFAULT NULL,
%%   `flashPlayerVersion` int(11) DEFAULT NULL,
%%   `connectType` int(11) DEFAULT NULL,
%%   `gameServerName` varchar(255) DEFAULT NULL,
%%   `ipAddr` varchar(255) DEFAULT NULL,
%%   PRIMARY KEY (`id`)
%% );

%% DELIMITER $$
%% drop procedure if exists `insert_tbl_test`$$
%% create procedure `insert_tbl_test`(
%% IN inaccountId int(11),/**/
%% IN inaccountType int(11),/**/
%% IN inplayerId int(11),/**/
%% IN inheadHeroId int(11),/**/
%% IN inplayerLevel int(11),/**/
%% IN inplayerName varchar(255),/**/
%% IN inlogTime datetime,/**/
%% IN inclientVersion varchar(255),/**/
%% IN inclientType varchar(255),/**/
%% IN inissuers varchar(255),/**/
%% IN inflashPlayerVersion int(11),/**/
%% IN inconnectType int(11),/**/
%% IN ingameServerName varchar(255),/**/
%% IN inipAddr varchar(255),/**/
%% IN inonOffType int(11) /**/
%% )
%% BEGIN
%% insert into `tbl_on_off`(
%% `accountId`, /**/
%% `accountType`, /**/
%% `playerId`, /**/
%% `headHeroId`, /**/
%% `playerLevel`, /**/
%% `playerName`, /**/
%% `logTime`, /**/
%% `clientVersion`, /**/
%% `clientType`, /**/
%% `issuers`, /**/
%% `flashPlayerVersion`, /**/
%% `connectType`, /**/
%% `gameServerName`, /**/
%% `ipAddr`, /**/
%% `onOffType` /**/
%% ) values(
%% inaccountId,/**/
%% inaccountType,/**/
%% inplayerId,/**/
%% inheadHeroId,/**/
%% inplayerLevel,/**/
%% inplayerName,/**/
%% inlogTime,/**/
%% inclientVersion,/**/
%% inclientType,/**/
%% inissuers,/**/
%% inflashPlayerVersion,/**/
%% inconnectType,/**/
%% ingameServerName,/**/
%% inipAddr,/**/
%%  inonOffType /**/
%% );
%% END$$
%% DELIMITER ;

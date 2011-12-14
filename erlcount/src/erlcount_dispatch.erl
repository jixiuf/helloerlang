-module(erlcount_dispatch).
-behaviour(gen_fsm).
-export([start_link/0,init/1]).
-export([handle_info/3,dispatching/2,listening/2,handle_event/3 ,handle_sync_event/4,terminate/3,code_change/4]).
-export([complete/4]).


-define(PoolName,erlcount).                     %erlcount 模块使用的pool的名字
%% regexs 格式为[{regex,count},...]
-record(data,{regexs=[],refs=[]}).

start_link()->
    io:format("erlcount dispatching starting... ~n",[]),
    gen_fsm:start_link(?MODULE,[],[])
    .

init([])->
    io:format("erlcount dispatching initing.... ~n",[]),
    {ok,Dir}=application:get_env(directory),
    {ok,Regexps}= application:get_env(regex),
    {ok,MaxFiles}= application:get_env(max_files),
    %% 启动一个pool ,ppool,是另一个例程 ，不是系统自带的
    io:format("starting a erlcount instance in ppool.... ~n",[]),
    ppool:start_pool(?PoolName,MaxFiles,{erlcount_counter,start_link,[]}),
    io:format("started a erlcount instance in ppool.... ~n",[]),
    case lists:all( fun valid_regexp/1, Regexps) of
        true->                                  %如果所有的正则表达示都合法
            self() ! {start,Dir},
            {ok, dispatching, #data{regexs=[{R,0}||R <- Regexps]}}; %构造成{Re,Count} 的形式
        false ->
            {stop,invalid_regex}
    end
        .
%% init之后处于 dispatching 状态 ，
handle_info({start,Dir},State,Data)->
    gen_fsm:send_event(self(),erlcount_lib:find_dir(Dir)), %发送一个{continue,File,Con} ,或done信息.
    {next_state,State,Data};
handle_info(Msg,State,Data) ->
    io:format("Unexcepted Msg:~p~n",[Msg]),
    {next_state,State,Data} .

dispatching({continue,File,ContinuationFun },Data=#data{regexs=Regexps,refs=Refs})->
    io:format("dispatching...~n",[]),
    F = fun ({Re,_Count},NewRefs)->
                Ref = make_ref(),
                io:format("function in dispatching() is called ,the reg is :~p~n",[Re]),
                ppool:async_queue(?PoolName,[self(),File,Ref,Re]),
                [Ref|NewRefs]
        end ,
    NewRefs= lists:foldl(F,Refs,Regexps),
    %%CPS 式编程，不同于依靠函数的返回值，它会把函数作为参数传过来，以便随后 调用 而非返回一个值 后，让对方对这个值进行处理
    ContinuationFun(),                          %在目录中继续递归寻找一下个erl.文件。
    {next_state,dispatching,Data#data{refs=NewRefs}};
dispatching(done,Data=#data{}) ->
    io:format("got 'done' message ,but maybe some process still handling files now ,so now changed to listening state...~n",[]),
    %% {next_state,listening,Data}
    listening(done,Data)
.
%% 当refs 为空时，才说明 所有文件都已经处理完毕，
listening(done,_Data=#data{refs=[],regexs=Regexs})->
    io:format("listening...~n",[]),
    [io:format("~p:~p,~n",[Re,Count])|| [Re,Count] <- Regexs],
    {stop,normal ,done};
listening(done,Data=#data{})->
    io:format("listening...~n",[]),
    {next_state,listening,Data}
        .

handle_event({complete,Regex,Ref,Count},State,Data=#data{regexs=Regexs,refs=Refs})->
    io:format("handle_global event for fsm...~n",[]),
    {Regex,OldCount}=lists:keyfind(Regex,1,Regexs),
    NewRegexs=lists:keyreplace(Regex,1,Regexs,{Regex,OldCount+Count}),
    NewRefs = lists:delete(Ref,Refs),
    NewData= Data#data{regexs=NewRegexs,refs=NewRefs},
    case State of
        listening->
            listening(done,NewData);
        dispatching ->
            {next_state,dispatching, NewData}
    end
        .
handle_sync_event(Msg, _From, StateName, StateData)->
    io:format("Unexpected  sync Msg:~p~n",[Msg]),
    {next_state,StateName,StateData}
        .

terminate(_Reason, _StateName, _StateData)->
    ok .
code_change(_,_,_,_)->
    io:format("code_changed.~n",[]),
    ok.
%% interface API
complete(Pid,Regex,Ref,Count)->
    gen_fsm:send_all_state_event(Pid,{complete,Regex,Ref,Count})
        .
%% private fun
valid_regexp(Re) ->
    try re:run("", Re) of
        _ -> true
    catch
        error:badarg -> false
    end.

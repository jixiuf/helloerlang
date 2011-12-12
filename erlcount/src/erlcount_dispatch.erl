-module(erlcount_dispatch).
-behaviour(gen_fsm).

-define(PoolName,erlcount).                     %erlcount 模块使用的pool的名字
%% regexs 格式为[{regex,count},...]
-record(data,{regexs=[],refs=[]}).

start_link()->
    io:format("erlcount fsm starting... ~n",[]),
    gen_fsm:start_link(?MODULE,[],[])
    .

init([])->
    {ok,Dir}=application:get_env(directory),
    {ok,Regexps}= application:get_env(regexps),
    {ok,MaxFiles}= application:get_env(max_files),
    %% 启动一个pool ,ppool,是另一个例程 ，不是系统自带的
    ppool:start_pool(?PoolName,MaxFiles,erlcount_counter,start_link,[]),
    case lists:all( fun valid_regexp/1, Regexps) of
        true->                                  %如果所有的正则表达示都合法
            self() ! {start,Dir},
            {ok, dispatching, #data{regexs=[{R,0}||R <- Regexps]}}; %构造成{Re,Count} 的形式
        false ->
            {stop,invalid_regex}
    end
        .

handle_info({start,Dir},State,Data)->
    gen_fsm:send_event(self(),erlcount_lib:find_dir(Dir)),
    {next_state,State,Data}
        .

complete (Pid,Regex,Ref,Count)->
    gen_fsm:send_all_state_event(Pid,{complete,Regex,Ref,Count})
    .
valid_regexp(Re) ->
    try re:run("", Re) of
        _ -> true
    catch
        error:badarg -> false
    end.

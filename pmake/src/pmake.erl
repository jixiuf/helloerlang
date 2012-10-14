%% 本来以为能提速的， 后来证明效果也不怎样，
%% 代码保留吧。
-module(pmake).
-behaviour(gen_fsm).
-export([make/0]).
%% util
-export([find_dir/1]).
%% internal api
-export([init/1,handle_info/3,dispatching/2,waiting_all_done/2,handle_event/3 ,handle_sync_event/4,terminate/3,code_change/4]).
-include_lib("kernel/include/file.hrl").

-record(state,{src=[],erl_opts=[],output_dir="",error=[],working=0}).
make()->
    gen_fsm:start(pmake,[],[]).

%% pmake.config
%% {src_dirs, ['src/']}.
%% {outdir, "ebin/"}.
%% {erl_opts, [{d,debug},debug_info,{i, "include"},{outdir, "ebin"},nowarn_unused_vars, nowarn_unused_function]}.
%% %% [debug_info, nowarn_unused_vars, nowarn_unused_function,native,{hipe,[o3]}]
init([])->
    {ok,Terms}=file:consult("pmake.config"),
    Srcs= proplists:get_value(src_dirs,Terms,["src/"]),
    OutputDir= proplists:get_value(outdir,Terms,"ebin/"),
    Erlopts= proplists:get_value(erl_opts,Terms,[]),
    OutputDir2=proplists:get_value(outdir,Erlopts,OutputDir),
    Erlopts2=[{outdir,OutputDir2}|proplists:delete(outdir,Erlopts)],
    filelib:ensure_dir(OutputDir),
    io:format("current dir:~p~n",[file:get_cwd()]),
    self() ! start,
    {ok, dispatching, #state{src=Srcs,erl_opts=Erlopts2,output_dir=OutputDir2}}
        .
%% init之后处于 dispatching 状态 ，
handle_info(start,State,S=#state{src=Src})->
    case Src of
        []->
            gen_fsm:send_event(self(),done), %done,此时fsm处于dispatching状态
            {next_state,State,S};
        [Src1|OSrcs] ->
            gen_fsm:send_event(self(),find_dir(Src1)) ,%发送一个{continue,File,Con} ,或done信息.
            {next_state,State,S#state{src=OSrcs}}
    end.

compile(F,Mon,S)->
    compile1(F,lists:reverse(F),S),
    gen_fsm:send_all_state_event(Mon,one_done)
    .
compile1(F,"crs.ppa."++_,#state{output_dir=OutputDir})->                      %a.app.src
    FileName=filename:basename(F),              %just file name  a.app.src
    AppName=filename:basename(FileName,".src"),  %a.app
    file:copy(F,filename:join([OutputDir,AppName])),
    io:format("copy ~p to ~p~n",[F,OutputDir]);
compile1(F,"lre."++_,#state{erl_opts=ErlOpts})->                            %erl
    io:format("compile ~p done ~n",[F]) ,
    c:c(F,ErlOpts);
    %% c:c(F,[{d,debug},debug_info,{i, "include"},{outdir, "ebin"},nowarn_unused_vars, nowarn_unused_function]);
compile1(_F,_,_S) ->
    ok.

dispatching({continue,File,ContinuationFun },S=#state{working=Cnt})->
    Mon=self(),
    spawn(fun()-> compile(File,Mon,S) end ),
    %%CPS 式编程，不同于依靠函数的返回值，它会把函数作为参数传过来，以便随后 调用 而非返回一个值 后，让对方对这个值进行处理
    gen_fsm:send_event(self(), ContinuationFun()),                          %在目录中继续递归寻找一下个erl.文件。
    {next_state,dispatching,S#state{working=Cnt+1}};
dispatching(done,S=#state{}) ->
    %% io:format("got 'done' message ,but maybe some process still handling files now ,so now changed to waiting_all_done state...~n",[]),
    %% {next_state,waiting_all_done,S}
    waiting_all_done(done,S).

waiting_all_done(done,_Data=#state{src=[],working=0})->
    io:format("all_done...~n",[]),
    {stop,normal ,done};
waiting_all_done(done,S=#state{working=0})->
    self()!start,                          %compile下一个src目录
    {next_state,dispatching,S};                 %转到dispatching等待下一个src的编译
waiting_all_done(done,S=#state{working=Cnt})->
    {next_state,waiting_all_done,S#state{working=Cnt-1}}
        .

handle_event(one_done,State,S=#state{working=Cnt})->
    case State of
        dispatching->
            {next_state,dispatching,S#state{working=Cnt-1}};
        waiting_all_done ->
            waiting_all_done(done,S)
        end.

handle_sync_event(_Msg, _From, StateName, StateData)->
    {next_state,StateName,StateData}
        .

terminate(_Reason, _StateName, _StateData)->
    init:stop()
     .
code_change(_,_,_,_)->
    io:format("code_changed.~n",[]),
    ok.


%% 返回值有二:
%%{continue,File,fun() -> dequeue_and_run(Queue)end }
%% done
find_dir(Directory)->
    find_dir(Directory,queue:new())
        .
%% private method  , use find_dir/1 instead.
find_dir(Directory,Queue)->
    %% io:format("find_dir/2 is called for ~p~n",[Directory]),
    {ok,F=#file_info{}}=file:read_file_info(Directory),
    case F#file_info.type of
        directory->
            handle_dir(Directory,Queue);
        regular ->
            handle_regular_file(Directory,Queue) ;
        _Other ->
            dequeue_and_run(Queue)
    end
        .
%% open direcotory and enqueue(入队) files in there
handle_dir(Directory,Queue)->
    %% io:format("handle directory: ~p~n",[Directory]),
    case file:list_dir(Directory) of
        {ok,[]}->
            dequeue_and_run(Queue);             %若当前目录为空目录 ，无子可入，继续从队列里取出一个进行处理
        {ok,Files} ->                           %若当前目录 非空，将子加入队列
            dequeue_and_run(enqueue_many(Directory,Files,Queue))
    end
        .

%% 将所有Files 入队
enqueue_many(Directory,FileNames,Queue)->
    %% io:format("enqueue many files in ~p...~n",[Directory]),
    F = fun (FileName,Q)->
                FullPath=filename:join(Directory,FileName),
                %% io:format("add ~p in ~n",[FullPath]) ,
                queue:in(FullPath,Q)
        end ,
    lists:foldl(F,Queue,FileNames)
        .

handle_regular_file(File,Queue)->
    {continue,File,fun() -> dequeue_and_run(Queue)end }
    %% case  filename:extension(File) of
    %%     ".erl"->
    %%         {continue,File,fun() -> dequeue_and_run(Queue)end };
    %%     _OtherExt ->
    %%         dequeue_and_run(Queue)
    %% end
        .
%% pop an item and run it
dequeue_and_run(Queue)->
    %% io:format("dequeue and running...~n",[]),
    case queue:out(Queue) of
        {empty,_}->
            done;
        {{value,File},NewQueue} ->
            %% io:format("pop item ~p~n",[File]),
            find_dir(File,NewQueue)             %递归调用
    end

        .

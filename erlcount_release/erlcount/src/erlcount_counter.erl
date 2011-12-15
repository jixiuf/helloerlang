-module(erlcount_counter).
-export([start_link/4,init/1]).
-export([handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-record(state,{dispatchingpid,file,ref,regexp}).


start_link(DispatchingPid,File,Ref,Re)->
    io:format("erlcount_counter starting... for ~p in ~p~n",[Re,File]),
    gen_server:start_link(?MODULE,[DispatchingPid,File,Ref,Re],[])
    .

init([DispatchingPid,File,Ref,Re])->
    io:format("erlcount_counter initing... for ~p in ~p~n",[Re,File]),
    self()!start,
    {ok,#state{dispatchingpid=DispatchingPid,file=File,ref=Ref,regexp=Re}}
        .

handle_call(_Request,_From,State)->
    {noreply, State}
        .

handle_cast(_Msg,State)->
    {noreply, State} .

handle_info(start,State=#state{file=File,regexp=Re,dispatchingpid=DispatchingPid,ref=Ref}) ->
    {ok,Bin}=file:read_file(File),
    Regexp_count_in_file=regexp_count(Re,Bin),
    file:close(File),
    erlcount_dispatch:complete (DispatchingPid,Re,Ref,Regexp_count_in_file),
    {stop,normal,State}
        ;
handle_info(_Info,State)->
    {noreply, State}.

terminate(_Reason,_State=#state{regexp=Re,file=File})->
    io:format("erlcount_counter handleing regexp:~p in file:~p done! ~n",[Re,File]),
    ok
        .

code_change(_Previous_Version,State,_Extra)->
    {ok,State}
        .

%%regexp_count("a","abab")
regexp_count(Regexp,Str)->
    case re:run(Str,Regexp,[global]) of
        nomatch->
            0;
        {match,List} ->
            length(List)
    end
        .

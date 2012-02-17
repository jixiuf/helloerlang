% -*- coding:utf-8 -*-
-module(gen_server_call_timeout).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
%%测试gen_server:call(Pid,msg,Timeout)中的Timeout
%% 表示发出这条请求后，在Timeout时间内要得到reply ,否则超时
%% 这个超时与init/1 handle_call等返回值里的Timeout不同
%%这里的超时是不能被handle_info/2处理的，
%% 并且会导致进程退出

%%代码逻辑在gen.erl 中do_call/4里
%% receive
%% {Mref, Reply} ->
%%     erlang:demonitor(Mref, [flush]),
%%     {ok, Reply};
%% {'DOWN', Mref, _, _, noconnection} ->
%%     exit({nodedown, Node});
%% {'DOWN', Mref, _, _, Reason} ->
%%     exit(Reason)
%% after Timeout ->
%%     erlang:demonitor(Mref),
%%     receive
%%     {'DOWN', Mref, _, _, _} -> true
%%     after 0 -> true
%%     end,
%%     exit(timeout) % 这里直接退出了
%% end

%%,如果代码中有gen_server:call/3 的代码，则要考虑会不会因为超时导致进程退出
%% 如果进程退出了，要采用什么策略,是否要进行重启,应该可以拼命 supersivor

start_link()->
    gen_server:start_link(?MODULE,[],[])
        .

init(S)->
    {ok, S}
        .

%% 这里故意sleep4s,使call超时
handle_call(_Request,_From,State)->
    timer:sleep(4000),
    {reply,hello, State}
        .

handle_cast(_Request,State)->
    {noreply, State} .

handle_info(_Info,State)->
    io:format("handle_info/2 got timeout msg.~n",[]) ,
    {noreply, State}.

terminate(_Reason,_State)->
    ok .

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .

%% {ok,P}=gen_server_call_timeout:start_link().
%%gen_server:call(P,hello,3000).
%%

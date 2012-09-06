-module(server_socket).
-export([get_socket/1,start_link/2]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-record(state,{listener,monitor_pid,socket}).
-include("../include/base_header.hrl").
-include("../include/debug.hrl").

start_link(ListenSocket,From)->
    gen_server:start_link(?MODULE,#state{listener=ListenSocket,monitor_pid=From},[])
        .

%%根据当前pid 获得关与关联的socket
get_socket(Pid)->
    {ok,Socket}=gen_server:call(Pid,get_socket),
    Socket.

init(S=#state{})->
    gen_server:cast(self(),accept),
    {ok, S}.

handle_call(get_socket,_From,State=#state{socket=Socket})->
    {reply,{ok,Socket},State};
handle_call(Request,_From,State)->
    ?DEBUG2("random handle_call msg: ~p~n",[Request]),
    {reply,ok, State}.

handle_cast(accept,State=#state{listener=ListenSocket,monitor_pid=From})->
    {ok, Socket} = gen_tcp:accept(ListenSocket), %
    ?DEBUG2("a new client is coming...~n",[]),
    gen_server:cast(From,{accepted,self()}),
    inet:setopts(Socket, [{active, once}]),
    {noreply,State#state{socket=Socket}};
handle_cast(Request,State)->
    ?DEBUG2("random handle_cast msg: ~p~n",[Request]),
    {noreply, State} .

handle_info({tcp, ClientSocket,Bin},State)when is_binary(Bin) ->
    ?DEBUG2("server handle data...~n",[]),
    case handle_data(Bin,ClientSocket)  of
        ok->inet:setopts(ClientSocket, [{active, once}]),
            {noreply,State};
        {error, Reason}->
            ?DEBUG2("exit with reason~p~n",[Reason]) ,
            handle_tcp_closed(ClientSocket),
            {stop,Reason , State}
    end;
handle_info({tcp_closed,Socket},State)->
    ?DEBUG("tcp_closed with reason ~n:"),
    handle_tcp_closed(Socket),
    {stop,tcp_closed , State};
handle_info({tcp_error, Socket, Reason},State)->
    ?DEBUG2("tcp_error with reason ~p~n:",[Reason]),
    handle_tcp_closed(Socket),
    {stop,tcp_error , State};
handle_info(Request,State)->
    %% ?DEBUG2("handle random msg ~p~n",[Other]), %%
    %% handle(ClientSocket,From) %%
    ?DEBUG2("random handle_info msg: ~p~n",[Request]),
    {noreply, State} .

terminate(Reason,_State)->
    ?DEBUG2("terminated with reason~p~n",[Reason]) ,
    ok .

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .

handle_tcp_closed(ClientSocket)->
    ?DEBUG2(" tcp_closed:~p!~n",[ClientSocket]),
    gen_tcp:close(ClientSocket),                 %try to close client socket,
    %% exit(),退出， 督程收到信号，在那里进行业务逻辑相关清理工作
    exit(tcp_closed).

handle_data(Bin,ClientSocket) ->
    EncodeData=
        try
            {ok,C2SProtocol}=protocol_decode:decode(Bin) ,
            S2CProtocols=protocol_handle:handle(C2SProtocol),
            [protocol_encode:encode(S2CProtocol)||S2CProtocol<-S2CProtocols]
        catch
            throw:Msg->
                ?DEBUG2("~p:handle_data/2 error with reason:~p~n",[?MODULE,Msg]) ,
                protocol_encode:encode_server_error();
            error:ErrorId ->
                ?DEBUG2("~p:handle_data/2 error with reason:~p~n",[?MODULE,ErrorId]) ,
                protocol_encode:encode_server_error()
        end,
    gen_tcp:send(ClientSocket,EncodeData).

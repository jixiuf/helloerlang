-module(client).

-export([close/1,echo/2]).
-export([connect/2]).
%% internel
-export([do_recv/1]).

-include_lib("include/base_header.hrl").
-include_lib("include/debug.hrl").


-define(TCP_OPTS,[binary,{active,false},{reuseaddr,true},{packet ,?C2S_TCP_PACKET}]).

connect(Host,Port) ->
    case gen_tcp:connect(Host, Port,?TCP_OPTS ) of
        {ok,ServerSocket}->
            Pid= spawn(?MODULE,do_recv,[ServerSocket]),
            register(?MODULE,Pid),              %pid of sending msg to server
            gen_tcp:controlling_process(ServerSocket, Pid),
            {ok,ServerSocket}
            %% timer:sleep(infinity)
                ;
        {error,Reason} ->
            {error,Reason}
    end
        .
do_recv(ServerSocket)->
    ?DEBUG2("client do recving...~n",[]),
    inet:setopts(ServerSocket, [{active, once}]),
    receive
        {send,Bin,Socket}->
            ?DEBUG2("client sending data to server ...~n",[]) ,
            gen_tcp:send(Socket,Bin),          %send Bin to ServerSocket
            do_recv(ServerSocket);
        {tcp, ServerSocket, Bin}->
            ?DEBUG2("client handle command ~n",[]),
            handle_command(Bin,ServerSocket),
            do_recv(ServerSocket);
        {tcp_closed, ServerSocket} ->
            ?DEBUG2("client tcp_closed!!!!!~n",[]),
            gen_tcp:close(ServerSocket),
            exit(normal)
            %% do_recv(ServerSocket)
             ;
        {tcp_error, ServerSocket, Reason} ->
            ?DEBUG2("error ~p~n",[Reason]) ,
            {error,Reason}
    end
    %% case   gen_tcp:recv(ServerSocket,0) of
    %%     {send,Bin,Socket}->
    %%         ?DEBUG2("client sending data to server ...~n",[]) ,
    %%         %% gen_tcp:send(Socket,Bin),          %send Bin to ServerSocket
    %%         do_recv(ServerSocket);
    %%     {ok,Bin}->
    %%         handle_command(Bin,ServerSocket),
    %%         do_recv(ServerSocket);
    %%     {error,Reason} ->
    %%         ?DEBUG2("error ~p~n",[Reason]) ,
    %%         {error,Reason};
    %%     OtherMsg ->
    %%         ?DEBUG2("OtherMsg:~p~n",[OtherMsg]),
    %%         do_recv(ServerSocket)
    %% end
        .
handle_command(<<?S2C_PROTOCOL_ECHO:?S2C_PROTOCOL_LENGTH,ErrorId:?S2C_ERROR_ID_LENGTH,EchoMsg/binary>>,_ServerSocket)-> %1:32 表示echo
    ?INFO2("errorid:~p(0==success)",[ErrorId]),
    {MsgBody,_OtherBin}=server_util:decode_str(EchoMsg),
    ?INFO2("client get msg from server and server said :~p~n",[MsgBody])
.

echo(Socket,Msg) when is_list(Msg)->            %Msg is string
    MsgBin=server_util:encode_str(Msg),
    Bin= <<?C2S_PROTOCOL_ECHO:?C2S_PROTOCOL_LENGTH,MsgBin/binary>>,
    whereis(?MODULE) ! {send ,Bin,Socket},
    ok .

close(Socket)->
    ok = gen_tcp:close(Socket).

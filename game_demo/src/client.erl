-module(client).
-export([close/1,echo/2]).
-export([connect/2]).

%% internel
-export([do_recv/1]).
-include_lib("base_header.hrl").
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
    chat_log:debug("client do recving...~n",[]),
    inet:setopts(ServerSocket, [{active, once}]),
    receive
        {send,Bin,Socket}->
            chat_log:debug("client sending data to server ...~n",[]) ,
            gen_tcp:send(Socket,Bin),          %send Bin to ServerSocket
            do_recv(ServerSocket);
        {tcp, ServerSocket, Bin}->
            chat_log:debug("client handle command ~n",[]),
            handle_command(Bin,ServerSocket),
            do_recv(ServerSocket);
        {tcp_closed, ServerSocket} ->
            chat_log:debug("client tcp_closed!!!!!~n",[]),
            gen_tcp:close(ServerSocket),
            exit(normal)
            %% do_recv(ServerSocket)
             ;
        {tcp_error, ServerSocket, Reason} ->
            chat_log:debug("error ~p~n",[Reason]) ,
            {error,Reason}
    end
    %% case   gen_tcp:recv(ServerSocket,0) of
    %%     {send,Bin,Socket}->
    %%         io:format("client sending data to server ...~n",[]) ,
    %%         %% gen_tcp:send(Socket,Bin),          %send Bin to ServerSocket
    %%         do_recv(ServerSocket);
    %%     {ok,Bin}->
    %%         handle_command(Bin,ServerSocket),
    %%         do_recv(ServerSocket);
    %%     {error,Reason} ->
    %%         io:format("error ~p~n",[Reason]) ,
    %%         {error,Reason};
    %%     OtherMsg ->
    %%         io:format("OtherMsg:~p~n",[OtherMsg]),
    %%         do_recv(ServerSocket)
    %% end
        .
handle_command(<<1:32,EchoMsg/binary>>,_ServerSocket)-> %1:32 表示echo
    chat_log:info("client get msg from server and server said :~p~n",[binary_to_list(EchoMsg)])
.

echo(Socket,Msg) when is_list(Msg)->            %Msg is string
    whereis(?MODULE) ! {send ,util:binary_concat([<<1:32>>,Msg]),Socket},
    ok .

close(Socket)->
    ok = gen_tcp:close(Socket).

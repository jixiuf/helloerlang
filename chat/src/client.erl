-module(client).
-export([do_recv/1,close/1,echo/2,connect/2]).


connect(Host,Port) ->
    case     gen_tcp:connect(Host, Port, [binary]) of
        {ok,ServerSocket}->
            Pid= spawn(?MODULE,do_recv,[ServerSocket]),
            gen_tcp:controlling_process(ServerSocket, Pid),
            %% gen_tcp:recv(ServerSocket,0)
            {ok,ServerSocket}
            %% timer:sleep(infinity)
                ;
        {error,Reason} ->
            {error,Reason}
    end
        .
do_recv(ServerSocket)->
    io:format("helllllll~n",[]),
    case   gen_tcp:recv(ServerSocket,0) of
        {ok,Bin}->
            case util:read_int32(Bin) of
                {Int_Of_Bit,CommandBody} when is_integer(Int_Of_Bit) and is_binary(CommandBody)->
                    case CommandBody of
                        <<Command:Int_Of_Bit,Tail/binary>>->
                            handle_command(<<Command:Int_Of_Bit>>,Tail,ServerSocket),
                            do_recv(ServerSocket);
                        _Other ->
                            do_recv(ServerSocket)
                    end

                        ;
                _Other ->
                    do_recv(ServerSocket)
            end ;
        {error,Reason} ->
            {error,Reason}
        end
    .
handle_command(<<"echo">>,Bin,_ServerSocket)->
    io:format("server said:~p~n",[binary_to_list(Bin)])
    .

echo(Socket,Msg) when is_list(Msg)->            %Msg is string
    ok = gen_tcp:send(Socket,iolist_to_binary([util:encode_command("echo"),Msg]) )
        .
close(Socket)->
    ok = gen_tcp:close(Socket).

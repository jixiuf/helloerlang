-module(emc).
-behaviour(gen_server).
-export([to_list/1,to_binary/1,to_integer/1]).
-export([start_link/1,start_link/0,connect/2]).
-export([quit/0,version/0,flush_all/0,touch_value/2,
         stats_sizes/0,stats_items/0,stats/0,
         dec_value/2,inc_value/2,
         delete_value/1,get_values/1,
         get_values_list/1,get_value/1,get_value_list/1,
         prepend_value/2, append_value/2,
         cas_value/5,cas_value/4,cas_value/3,
         replace_value/4,replace_value/3,replace_value/2,
         add_value/4,add_value/3,add_value/2,
         set_value/4,set_value/3,set_value/2]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

-export([test_all/0]).


-define(SERVER_NAME_RELEASE,"127.0.0.1").
-define(SERVER_PORT_RELEASE,9180).

getServerAddress()->
    MemServerAddr =?SERVER_NAME_RELEASE,
    MemServerPort=?SERVER_PORT_RELEASE,
    {MemServerAddr,MemServerPort}
    .


set_value(Key,Value)->
    gen_server:call(?MODULE,{set ,to_list(Key),to_list(0),to_list(0),to_binary(Value)}).
set_value(Key,Flag,Value)->
    gen_server:call(?MODULE,{set ,to_list(Key),to_list(Flag),to_list(0),to_binary(Value)}).
set_value(Key,Flag,Exptime,Value) when is_integer(Flag),is_integer(Exptime)->
    gen_server:call(?MODULE,{set ,to_list(Key),to_list(Flag),to_list(Exptime),to_binary(Value)}).

add_value(Key,Value)->
    gen_server:call(?MODULE,{add ,to_list(Key),to_list(0),to_list(0),to_binary(Value)}).
add_value(Key,Flag,Value)->
    gen_server:call(?MODULE,{add ,to_list(Key),to_list(Flag),to_list(0),to_binary(Value)}).
add_value(Key,Flag,Exptime,Value) when is_integer(Flag),is_integer(Exptime)->
    gen_server:call(?MODULE,{add ,to_list(Key),to_list(Flag),to_list(Exptime),to_binary(Value)}).

replace_value(Key,Value)->
    gen_server:call(?MODULE,{replace ,to_list(Key),to_list(0),to_list(0),to_binary(Value)}).
replace_value(Key,Flag,Value)->
    gen_server:call(?MODULE,{replace ,to_list(Key),to_list(Flag),to_list(0),to_binary(Value)}).
replace_value(Key,Flag,Exptime,Value) when is_integer(Flag),is_integer(Exptime)->
    gen_server:call(?MODULE,{replace ,to_list(Key),to_list(Flag),to_list(Exptime),to_binary(Value)}).


cas_value(Key,CasUnique,Value)->
    gen_server:call(?MODULE,{cas ,to_list(Key),to_list(0),to_list(0),to_list(CasUnique), to_binary(Value)}).
cas_value(Key,Flag,CasUnique,Value)->
    gen_server:call(?MODULE,{cas ,to_list(Key),to_list(Flag),to_list(0),to_list(CasUnique),to_binary(Value)}).
cas_value(Key,Flag,Exptime,CasUnique,Value) when is_integer(Flag),is_integer(Exptime)->
    gen_server:call(?MODULE,{cas ,to_list(Key),to_list(Flag),to_list(Exptime),to_list(CasUnique),to_binary(Value)}).

append_value(Key,Value)->
    gen_server:call(?MODULE,{append ,to_list(Key),to_binary(Value)}).

prepend_value(Key,Value)->
    gen_server:call(?MODULE,{prepend ,to_list(Key),to_binary(Value)}).

get_value_list(Keys) when is_list(Keys)->
    KeyList=[to_list(Key)||Key<-Keys],
    gen_server:call(?MODULE,{get,string:join(KeyList," ")}).
get_value(Key)->
    gen_server:call(?MODULE,{get,to_list(Key)}).

get_values_list(Keys) when is_list(Keys)->
    KeyList=[to_list(Key)||Key<-Keys],
    gen_server:call(?MODULE,{gets,string:join(KeyList," ")}).

get_values(Key)->
    gen_server:call(?MODULE,{gets,to_list(Key)}).
delete_value(Key)->
    gen_server:call(?MODULE,{delete,to_list(Key)}).

inc_value(Key,OffSet) when is_integer(OffSet)->
    gen_server:call(?MODULE,{inc,to_list(Key),OffSet}).

dec_value(Key,OffSet) when is_integer(OffSet)->
    gen_server:call(?MODULE,{dec,to_list(Key),OffSet}).

touch_value(Key,OffSet) when is_integer(OffSet)->
    gen_server:call(?MODULE,{touch,to_list(Key),OffSet}).

stats()->
    gen_server:call(?MODULE,{stats}).
stats_items()->
    gen_server:call(?MODULE,{stats_items}).

stats_sizes()->
    gen_server:call(?MODULE,{stats_sizes}).
flush_all()->
    gen_server:call(?MODULE,{flush_all}).
version()->
    gen_server:call(?MODULE,{version}).
quit()->
    gen_server:call(?MODULE,{quit}).

connect(Host,Port)->
        gen_server:call(?MODULE,{connect,Host,Port}).

start_link()->
    {MemServerAddr,MemServerPort}=getServerAddress(),
    start_link([MemServerAddr,MemServerPort]).

start_link([MemServerAddr,MemServerPort])->
    gen_server:start_link({local,?MODULE},?MODULE,[MemServerAddr,MemServerPort],[]).
init([MemServerAddr,MemServerPort])->
    connect_srv(MemServerAddr,MemServerPort).
handle_call({set,Key,Flag,Exptime,Value},_From,Socket)->
    process_store_cmd(Socket,"set",Key,Flag,Exptime,Value);
handle_call({add,Key,Flag,Exptime,Value},_From,Socket)->
    process_store_cmd(Socket,"add",Key,Flag,Exptime,Value);
handle_call({replace,Key,Flag,Exptime,Value},_From,Socket)->
    process_store_cmd(Socket,"replace",Key,Flag,Exptime,Value);
handle_call({cas,Key,Flag,Exptime,CasUnique,Value},_From,Socket)->
    process_cas_cmd(Socket,Key,Flag,Exptime,CasUnique,Value);
handle_call({append,Key,Value},_From,Socket)->
    process_store_cmd(Socket,"append",Key,"0","0",Value);
handle_call({prepend,Key,Value},_From,Socket)->
    process_store_cmd(Socket,"prepend",Key,"0","0",Value);
handle_call({get,Key},_From,Socket)->
    NewSocket= send_cmd(Socket,iolist_to_binary([<<"get ">>,Key])),
    Reply= receive_parse(NewSocket),
    case Reply of
        {value,[]}->
            {reply,{not_found},NewSocket};
        _ ->
            {reply,Reply,NewSocket}
    end;
handle_call({gets,KeyList},_From,Socket)->
    NewSocket= send_cmd(Socket,iolist_to_binary([<<"gets ">>,KeyList])),
    Reply= receive_parse(NewSocket),
    case Reply of
        {value,[]}->
            {reply,{not_found},NewSocket};
        _ ->
            {reply,Reply,NewSocket}
    end;
handle_call({delete,Key},_From,Socket)->
    process_common_cmd(Socket,iolist_to_binary(["delete ",Key]));
handle_call({inc,Key,OffSet},_From,Socket)->
    process_common_cmd(Socket,iolist_to_binary(["inc ",Key,<<" ">>,to_list(OffSet)]));
handle_call({dec,Key,OffSet},_From,Socket)->
    process_common_cmd(Socket,iolist_to_binary(["dec ",Key,<<" ">>,to_list(OffSet)]));
handle_call({touch,Key,Exptime},_From,Socket)->
    process_common_cmd(Socket,iolist_to_binary(["touch ",Key,<<" ">>,Exptime]));
handle_call({stats},_From,Socket)->
    process_common_cmd(Socket,iolist_to_binary(["stats"]));
handle_call({stats_items},_From,Socket)->
    process_common_cmd(Socket,iolist_to_binary(["stats_items"]));
handle_call({stats_sizes},_From,Socket)->
    process_common_cmd(Socket,iolist_to_binary(["stats_sizes"]));
handle_call({flush_all},_From,Socket)->
    process_common_cmd(Socket,iolist_to_binary(["flush_all"]));
handle_call({version},_From,Socket)->
    process_common_cmd(Socket,iolist_to_binary(["version"]));
handle_call({quit},_From,Socket)->
    disconnect(Socket),
    {reply,{ok,closed},Socket};
handle_call({connect,Host,Port},_From,_Socket)->
    {ok,NewSocket}=connect_srv(Host,Port),
    {reply,{ok,connected},NewSocket};
handle_call(_RandomReq,_From,Socket) ->
    {reply,ok,Socket}.

handle_cast(_Request,State)-> {noreply, State} .

handle_info(_Info,State)-> {noreply, State}.

terminate(_Reason,Socket)->
    disconnect(Socket).

code_change(_Previous_Version,State,_Extra)->
    {ok,State} .



process_store_cmd(Socket,Cmd,Key,Flag,Exptime,Value)->
    Bytes=integer_to_list(byte_size(Value)),
    NewSocket=send_cmd(Socket,
                       iolist_to_binary([to_binary(Cmd),<<" ">>,Key,<<" ">>,Flag,<<" ">>,Exptime,<<" ">>,Bytes,<<"\r\n">>,Value])
                      ),
    Reply=receive_parse(NewSocket),
    {reply,Reply,NewSocket}.

process_cas_cmd(Socket,Key,Flag,Exptime,CasUnique,Value)->
    Bytes=integer_to_list(byte_size(Value)),
    NewSocket=send_cmd(Socket,
                       iolist_to_binary([<<"cas ">>,Key,<<" ">>,Flag,<<" ">>,Exptime,<<" ">>,Bytes,<<" ">>,CasUnique,<<"\r\n">>,Value])
                      ),
    Reply=receive_parse(NewSocket),
    {reply,Reply,NewSocket}.
process_common_cmd(Socket,Cmd)->
    NewSocket=send_cmd(Socket,Cmd),
    Reply=receive_parse(NewSocket),
    {reply,Reply,NewSocket}.

send_cmd(Socket,Cmd)->
    case gen_tcp:send(Socket,<<Cmd/binary,"\r\n">>) of
        ok->
            Socket;
        {error,Reason} ->
            io:format("send_cmd error with reason~p~n",[Reason]) ,
            {MemServerAddr,MemServerPort}=getServerAddress(),
            io:format("reconnect mem server~n",[]) ,
            {ok,NewSocket}=connect_srv(MemServerAddr,MemServerPort),
            send_cmd(NewSocket,Cmd)
    end.
receive_parse(Socket)->
    case receive_data(Socket) of
        {error,Reason}->
            {error,Reason};
        Data ->
            parse_receive(Socket,Data,[])
        end

    .
receive_data(Socket)->
    case gen_tcp:recv(Socket,0) of
        {ok,Bin}->Bin;
        {error,Reason} ->
            io:format("gen_tcp:reev error with reason:~p~n",[Reason]) ,
            {error,Reason}
        end    .

retrieve_line(BinaryData)->
    case binary:match(BinaryData,[<<"\r\n">>]) of
        nomatch->
            {null,BinaryData};
        {Pos,Len} ->
            Line=binary:part(BinaryData,{0,Pos}),
            TailPos=Pos+Len,
            Tail=binary:part(BinaryData,{TailPos,byte_size(BinaryData)-TailPos}),
            {Line,Tail}
    end.

retrieve_value(BinaryData,RequireLen)->
    BinarySize=byte_size(BinaryData),
    if RequireLen=<BinarySize ->
            {binary:part(BinaryData,{0,RequireLen}),
             binary:part(BinaryData,{RequireLen,BinarySize-RequireLen})
            };
       true ->
            {null,BinaryData}
    end.
parse_value(Socket,Data,Size)->
    case retrieve_value(Data,Size) of
        {null,Tail}->
            Bin= receive_data(Socket),
            parse_value(Socket,list_to_binary([Tail,Bin]),Size);
        {Value,Tail} ->
            ValueData=binary:part(Value,{0,byte_size(Value)-2}),
            {ValueData,Tail}
    end.
parse_receive(Socket,Data,Values)->
    case retrieve_line(Data) of
        {null,Tail}->
            Bin=receive_data(Socket),
            parse_receive(Socket,list_to_binary([Tail,Bin]),Values);
        {Line,Tail} ->
            case parse_line(Line) of
                {end_data}->{values,Values};
                {value,Key,Flag,Bytes} ->
                    {ValueData,T}=parse_value(Socket,Tail,Bytes+2),
                    Value={value,Key,Flag,ValueData},
                    parse_receive(Socket,T,Values++[Value]);
                {valueWithCas,Key,Flag,Cas,Bytes} ->
                    {ValueData,T}=parse_value(Socket,Tail,Bytes+2),
                    Value={valueWithCas,Key,Flag,Cas,ValueData},
                    parse_receive(Socket,T,Values++[Value]);
                {stat,Name,Value} ->
                    Stat={Name,Value},
                    parse_receive(Socket,Tail,Values++[Stat]);
                {new_value,Value} ->
                    {ok,Value};
                {server_error,Value} ->
                    {server_error,Value};
                Result ->
                    Result
            end
    end.
parse_line(Line) ->
    case Line of
        <<"VALUE ",Data/binary>> ->
            Parse= io_lib:fread("~s ~d ~d",binary_to_list(Data)),
            {ok,[Key,Flag,Bytes],Last}=Parse,
            case length(Last)>0 of
                true->
                    ParseLast=io_lib:fread("~d",binary_to_list(Data)),
                    {ok,[Cas],_Last}=ParseLast,
                    {valueWithCas,Key,Flag,Cas,Bytes};
                false ->
                    {value,Key,Flag,Bytes}
            end;
        <<"STAT ",Data/binary>> ->
            Parse= io_lib:fread("~s ~s",binary_to_list(Data)),
            {ok,[Name,Value],_Last}=Parse,
            {stat,Name,Value};
        <<"END">> ->
            {end_data};
        <<"ERROR">> ->
            {error};
        <<"CLIENT_ERROR ",Error/binary>> ->
            {client_error,Error};
        <<"SERVER_ERROR ",Error/binary>> ->
            {server_error,Error};
        <<"STORED">> ->
            {ok,stored};
        <<"NOT_STORED">> ->
            {not_stored};
        <<"EXISTS">> ->
            {exists};
        <<"NOT_FOUND">> ->
            {not_found};
        <<"DELETED">> ->
            {ok,deleted};
        <<"TOUCHED">> ->
            {ok,touched};
        <<"OK">> ->
            {ok};
        <<"VERSION ",Data/binary>> ->
            Parse= io_lib:fread("~s",binary_to_list(Data)),
            {ok,[Version],_Last}=Parse,
            {version,Version};
        Data ->
            Parse= io_lib:fread("~d",binary_to_list(Data)),
            {ok,[Value],_Last}=Parse,
            {new_value,Value}
    end.

connect_srv(MemServerAddr,MemServerPort)->
    case gen_tcp:connect(MemServerAddr,MemServerPort,[binary,{packet,0},{active,false}]) of
        {ok,Socket}->
            {ok,Socket};
        Error ->
            exit(Error)
    end
        .

disconnect(Socket) when is_port(Socket)->
    gen_tcp:close(Socket);
disconnect(_S) ->
    ok.


to_list(List) when is_list(List)->
    List;
to_list(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Int) when is_integer(Int) ->
    integer_to_list(Int).

to_binary(Bin) when is_binary(Bin)->
    Bin;
to_binary(List) when is_list(List) ->
    list_to_binary(List);
to_binary(Int) when is_integer(Int) ->
    list_to_binary(integer_to_list(Int));
to_binary(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
to_binary(Any) ->
    term_to_binary(Any).

to_integer(Int) when is_integer(Int)->
    Int;
to_integer(List) when is_list(List) ->
    list_to_integer(List);
to_integer(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).



test_all()->
    start_link(["127.0.0.1",11211]),
    set_value(1,0,0,"100"),
    get_value(1),
    inc_value(1,1),
    add_value(1,1,100).

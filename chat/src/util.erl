-module(util).
-export([binary_length_concat/2,split_binary_by_head_int_value/1,time_to_string/1,binary_concat/1,int32_2_binary/1,read_int32/1]).

%util function%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% read int32 from head of Bin data
%% return {intValue,TailBin}
read_int32(Bin)when is_binary(Bin)->
    case Bin of
        <<Int:32,T/binary>>->
            {Int,T} ;
        _O->
            io:format("messager format error!~n",[]),
            {error,message_format_error}
    end
        ;
read_int32(_NoneBin) ->
    {error,message_not_bin}.

%% Bin格式为:<<Size_Of_Part1:32,Part1,Tail>>
%% return {Part1,Tail}
%%split_binary_by_head_int_value(<<5:32,"helloworld">>)={<<"hello">>,<<"world">>}
split_binary_by_head_int_value(Bin) when is_binary(Bin)->
    {Len,Body} = read_int32(Bin),
    <<Header:Len/binary,Tail/binary>> = Body,
    {Header,Tail}.

%% return <<LengthOfBin1:32,Bin1,Bin2>>
binary_length_concat(Bin1,Bin2)->
    Len=size(Bin1),
    util:binary_concat([<<Len:32>>,Bin1,Bin2])
    .

%%int按网络字节流 转成binary
%%网络传输一般采用大端序big，也被称之为网络字节序，或网络序
%%而erlang 默认就是big
int32_2_binary(Int) when is_integer(Int)->
    <<Int:32>> ;
int32_2_binary(Bin) when is_binary(Bin) ->                          %若本就Bin ,直接返回
    Bin.

%% util:encode_command("echo").
%% util:encode_command(<<"echo">>).
%%协议规定 int32+command+commandbody (int32为command的长度（bit长度）)
%%此函数为 command 作参， int32+commnad作返回值
%% encode_command(StringCommand) when is_list( StringCommand)  ->
%%     Len_of_Byte = length(StringCommand),
%%     Len_of_Bit = Len_of_Byte * 8 ,
%%     iolist_to_binary([<<Len_of_Bit:32>>,StringCommand])
%%         ;
%% encode_command(BinCommand) when is_binary(BinCommand) ->
%%     Len_of_Bit = bit_size(BinCommand),
%%     iolist_to_binary([<<Len_of_Bit:32>>,BinCommand])
%%         .

%% %% return {BitLenOfBin,Bin}
%% %% @param BinCommand :the format  is  <int32+command>>
%% decode_command(BinCommand) when is_binary(BinCommand) ->
%%     <<BitLen:32,Bin/binary>>=BinCommand,
%%     {BitLen,Bin}
%% .

%% binary_concat(Bin1,Bin2) when is_binary(Bin1) and is_binary(Bin2)->
%%     iolist_to_binary([Bin1,Bin2])    ;
%% binary_concat(Bin1,String) when is_binary(Bin1) and is_list(String) ->
%%     iolist_to_binary([Bin1,list_to_binary(String)]);
%% binary_concat(String,Bin1) when is_binary(Bin1) and is_list(String) ->
%%     iolist_to_binary([Bin1,list_to_binary(String)]);
%% binary_concat(String1,String2) when is_list(String1) and is_list(String2) ->
%%     iolist_to_binary([list_to_binary(String1),list_to_binary(String2)]).

binary_concat(Bins)->
    binary_concat(Bins,<<"">>)
    .
binary_concat([],Result) ->
    Result;
binary_concat([H|T],Result) when is_binary(H) ->
    NewR=iolist_to_binary([Result,H]),
    binary_concat(T,NewR) ;
binary_concat([H|T],Result) when is_list(H) ->
    NewR=iolist_to_binary([Result,list_to_binary(H)]),
    binary_concat(T,NewR).





%% Time (as return by calendar:local_time() to string conversion.
%% "2012-01-06 09:55:12"
time_to_string({{Y,Mo,D},{H,Mi,S}}) ->
    String = io_lib:format( "~4.4w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",
                            [Y,Mo,D,H,Mi,S] ),

         lists:flatten(String).

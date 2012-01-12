-module(util).
-export([decode_command/1,binary_concat/2,encode_command/1,int32_2_binary/1,read_int32/1]).

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
encode_command(StringCommand) when is_list( StringCommand)  ->
    Len_of_Byte = length(StringCommand),
    Len_of_Bit = Len_of_Byte * 8 ,
    iolist_to_binary([<<Len_of_Bit:32>>,StringCommand])
        ;
encode_command(BinCommand) when is_binary(BinCommand) ->
    Len_of_Bit = bit_size(BinCommand),
    iolist_to_binary([<<Len_of_Bit:32>>,BinCommand])
        .

%% return {BitLenOfBin,Bin}
%% @param BinCommand :the format  is  <int32+command>>
decode_command(BinCommand) when is_binary(BinCommand) ->
    <<BitLen:32,Bin/binary>>=BinCommand,
    {BitLen,Bin}
        .

binary_concat(Bin1,Bin2)->
    iolist_to_binary([Bin1,Bin2])    .

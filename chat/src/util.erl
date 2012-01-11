-module(util).
-export([int32_2_binary/1,read_int32/1]).

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

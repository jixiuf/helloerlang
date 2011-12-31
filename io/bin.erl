-module(bin).
-export([i32/1]).
-export([test/0]).


%% bin-->int32,int32->bin
%% bsl binary switch left
%% bsr binary switch right
%% band binary and

i32(B) when is_binary(B) ->
    i32(binary_to_list(B, 1, 4));
i32([X1, X2, X3, X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4;
i32(Int) when is_integer(Int) ->
    [(Int bsr 24) band 255,
     (Int bsr 16) band 255,
     (Int bsr  8) band 255,
     Int band 255].


test()->
    %% emacs int-2-binary/interactive
    %% a =97 ,b=98,c =99 ,d=100
    %% 97  00000000000000000000000001100001
    %% 98  00000000000000000000000001100010
    %% 99  00000000000000000000000001100011
    %% 100 00000000000000000000000001100100
    %% 1633837924  2#1100001011000100110001101100100
    IntValue= i32(<<"abcd">>),                            %bin-->int32

    io:format("~p~n",[IntValue]),
    Bin=i32(IntValue),
    io:format("~p~n",[Bin])
    .

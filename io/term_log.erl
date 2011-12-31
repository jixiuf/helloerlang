-module(term_log).
-export([test/0]).
-export([log/2]).

%% write Term to file
%% term_to_binary

%% bin-->int32,int32->bin
%% bsl binary switch left
%% bsr binary switch right
%% band binary and

i32(Bin) when is_binary(Bin) ->
    i32(binary_to_list(Bin, 1, 4));
i32([X1, X2, X3, X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4;
i32(Int) when is_integer(Int) ->
    [(Int bsr 24) band 255,                     %255 11111111
     (Int bsr 16) band 255,
     (Int bsr  8) band 255,
     Int band 255].

read_int32(File)->
    {ok,Bin}= file:read(File,4),
    i32(Bin)
    .

append_file(File,)

log(Term,IO)->
    file:write(IO,term_to_binary(Term))

.
test()->
    {ok,FileHandle}= file:open("term_log.out",[write,binary]),
    log(hello,FileHandle),
    file:close(FileHandle)
        .

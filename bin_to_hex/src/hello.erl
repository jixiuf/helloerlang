% Use: erlc hello.erl && erl -pa ./ebin -s hello run -s init stop -noshell

-module(hello).
-export([bin_to_hex/1]).

bin_to_hex(Bin)->
    bin_to_hex(Bin,[]).
%% erlang:integer_to_list
bin_to_hex(<<>>,Value)->
    lists:reverse(Value);
bin_to_hex(<<I:8,Bin/binary>>,Value)->
    Hex=integer_to_list(I,16),
    NewHex=case  length(Hex) of
        1->
            "0"++Hex;
        _->
            Hex
        end,
    bin_to_hex(Bin,[NewHex|Value]).

-module(record_to_json).
%% -export([encode_value/1]).
%%         .
%% encode_field(Record,Field,FieldValue) when is_integer(FieldValue)->
%%     [list_to_binary(atom_to_list(Field)),<<":">>,list_to_binary(integer_to_list(FieldValue))];
%% encode_field(Record,Field,FieldValue) when is_atom(FieldValue)->
%%     [list_to_binary(atom_to_list(Field)),<<":\"">>,list_to_binary(erlang:atom_to_list(FieldValue)),"\""];
%% encode_field(Record,Field,FieldValue) when is_list(FieldValue)->
%%     [list_to_binary(atom_to_list(Field)),<<":\"">>,list_to_binary(FieldValue),<<"\"">>];
%% encode_field(Record,Field,FieldValue) when is_binary(FieldValue)->
%%     [list_to_binary(atom_to_list(Field)),<<":">>,FieldValue] ;
%% encode_field(Record,Field,FieldValue) ->
%%     case is_record(FieldValue) of
%%         true->
%%             [list_to_binary(atom_to_list(Field)),<<":{">>,encode_record(FieldValue),<<"}">>];
%%         false ->
%%             exit(encode_record_error)
%%     end
%% .

%% encode_record(R)->
%%     case is_record(R) of
%%         true->
%%             Fields=lists:reverse(fields_info(R)),
%%             case Fields of
%%                 []->
%%                     <<"">>;
%%                 [LastField|TailFields]->
%%                     list_to_binary(
%%                       [[encode_field(R,F,get_value(R,F)),<<",">>]||F<-lists:reverse(TailFields)]++
%%                           [encode_field(R,LastField,get_value(R,LastField))]
%%                      )
%%             end
%%                 ;
%%         false ->
%%             exit(encode_record_error)
%%     end

%%         .

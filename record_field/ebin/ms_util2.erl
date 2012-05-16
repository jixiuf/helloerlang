%% This module automatically generated - do not edit

%%% This module provides utilities for use in building
%%% match specifications from records

-module(ms_util2).

-export([get_index/2,no_of_fields/1]).

no_of_fields(user) -> 3;
no_of_fields(Other) -> exit({error,"Invalid Record Name: "++Other}).


get_index(user,name)-> 1;
get_index(user,id)-> 2;
get_index(user,age)-> 3;
get_index(user,F) -> exit({error,"Record: user has no field called "++atom_to_list(F)});
get_index(Record,_Field) -> exit({error,"Invalid Record Name: "++Record}).

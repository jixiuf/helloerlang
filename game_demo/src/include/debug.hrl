%% -define(NOLOG,true).
-define(ML_FMT(F),"(~p:~p) "++(F)).
-define(ML_DATA(D),[?MODULE,?LINE]++(D)).

-ifdef(NOLOG).

    -define(DEBUG(F),F).
    -define(DEBUG2(F,D),{F,D}).

    -define(INFO(F),F).
    -define(INFO2(F,D),{F,D}).

    -define(ERROR(F),F).
    -define(ERROR2(F,D),{F,D}).
-else.
    -define(DEBUG(F),
            error_logger:info_msg(?ML_FMT("[D]"++F),?ML_DATA([]))).
    -define(DEBUG2(F,D),
            error_logger:info_msg(?ML_FMT("[D]"++F),?ML_DATA([D]))).

    -define(INFO(F),error_logger:info_msg(?ML_FMT("[I]"++F),?ML_DATA([]))).
    -define(INFO2(F,D),error_logger:info_msg(?ML_FMT("[I]"++F),?ML_DATA([D]))).

    -define(ERROR(F),error_logger:error_msg(?ML_FMT("[**E**]"++F),?ML_DATA([]))).
    -define(ERROR2(F,D),error_logger:error_msg(?ML_FMT("[**E**]"++F),?ML_DATA([D]))).
-endif.

%% -define(NOLOG,true).

-define(ML_FMT(F),"(~p:~p) "++(F)).
-define(ML_DATA(D),[?MODULE,?LINE]++(D)).

-ifdef(NOLOG).

    -define(DEBUG(F),ok).
    -define(DEBUG2(F,D),ok).

    -define(INFO(F),ok).
    -define(INFO2(F,D),ok).

    -define(ERROR(F),ok).
    -define(ERROR2(F,D),ok).
-else.
    -define(DEBUG(F),
            error_logger:info_msg(?ML_FMT("[D]"++F),?ML_DATA([]))).
    -define(DEBUG2(F,D),
            error_logger:info_msg(?ML_FMT("[D]"++F),?ML_DATA(D))).

    -define(INFO(F),error_logger:info_msg(?ML_FMT("[I]"++F),?ML_DATA([]))).
    -define(INFO2(F,D),error_logger:info_msg(?ML_FMT("[I]"++F),?ML_DATA(D))).

    -define(ERROR(F),error_logger:error_msg(?ML_FMT("[**E**]"++F),?ML_DATA([]))).
    -define(ERROR2(F,D),error_logger:error_msg(?ML_FMT("[**E**]"++F),?ML_DATA(D))).
-endif.

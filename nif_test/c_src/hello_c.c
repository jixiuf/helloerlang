#include "erl_nif.h"

static ERL_NIF_TERM hello_in_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] =
{
    {"hello", 0, hello_in_c}
};
/* hello_test 跟 hello_test.erl模块名要同 */
ERL_NIF_INIT(hello_test,nif_funcs,NULL,NULL,NULL,NULL)

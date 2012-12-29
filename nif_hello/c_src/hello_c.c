#include "erl_nif.h"
/* 有相关博文 */
/* http://wqtn22.iteye.com/blog/1603729 */

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
/* ERL_NIF_INIT宏展开之后,形如下面一段代码,nif_init()函数是入口函数, */
/* ErlNifEntry* nif_init(void); */
/* ErlNifEntry* nif_init(void) { */
/*   static ErlNifEntry entry = { 2, 3, */
/*                                "hello_test", */
/*                                sizeof(nif_funcs) / sizeof(*nif_funcs), */
/*                                nif_funcs, */
/*                                ((void *)0), */
/*                                ((void *)0), */
/*                                ((void *)0), */
/*                                ((void *)0), */
/*                                "beam.vanilla" }; */
/*   ; */
/*   return &entry; } */

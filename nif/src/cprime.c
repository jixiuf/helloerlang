#include <stdbool.h>
#include <math.h>
#include "erl_nif.h"

static bool isPrime(int i)
{
  int j;
  int t = sqrt(i) + 1;
  for(j = 2; j <= t; ++j)
    {
      if(i % j == 0)
        return false;
    }
  return true;
}

static ERL_NIF_TERM findPrime(ErlNifEnv *env, int argc, ERL_NIF_TERM argv[])
{
  int n;
  if(!enif_get_int(env, argv[0], &n))
    return enif_make_badarg(env);
  else
    {
      int i;
      ERL_NIF_TERM res = enif_make_list(env, 0);
      for(i = 2; i < n; ++i)
        {
          if(isPrime(i))
            res = enif_make_list_cell(env, enif_make_int(env, i), res);
        }
      return res;
    }
}

static ErlNifFunc nif_funcs[] = {
  {"findPrime", 1, findPrime}
};

ERL_NIF_INIT(prime, nif_funcs, NULL, NULL, NULL, NULL)

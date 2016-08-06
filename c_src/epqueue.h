#ifndef EPQUEUE_C_SRC_EZLIB_H_
#define EPQUEUE_C_SRC_EZLIB_H_

#include "erl_nif.h"

void nif_epqueue_free(ErlNifEnv* env, void* obj);
ERL_NIF_TERM nif_epqueue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_epqueue_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_epqueue_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_epqueue_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_epqueue_pop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_epqueue_peek(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif

#ifndef C_SRC_NIF_UTILS_H_
#define C_SRC_NIF_UTILS_H_

#include "erl_nif.h"

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM make_error(ErlNifEnv* env, const char* error);
ERL_NIF_TERM make_error(ErlNifEnv* env, ERL_NIF_TERM term);
ERL_NIF_TERM make_binary(ErlNifEnv* env, const char* buff, size_t length);

#endif  // C_SRC_NIF_UTILS_H_

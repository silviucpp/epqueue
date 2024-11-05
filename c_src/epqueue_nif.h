#ifndef C_SRC_EPQUEUE_NIF_H_
#define C_SRC_EPQUEUE_NIF_H_

#include "erl_nif.h"

struct atoms
{
    ERL_NIF_TERM atomOk;
    ERL_NIF_TERM atomError;
    ERL_NIF_TERM atomTrue;
    ERL_NIF_TERM atomFalse;
    ERL_NIF_TERM atomUndefined;
    ERL_NIF_TERM atomNotFound;

    ERL_NIF_TERM atomGlobalLock;
};

struct epqueue_data
{
    ErlNifResourceType* resPQueueInstance;
    ErlNifResourceType* resPQueueItem;
};

extern atoms ATOMS;

#endif  // C_SRC_EPQUEUE_NIF_H_

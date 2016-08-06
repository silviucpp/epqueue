#ifndef EPQUEUE_C_SRC_EZLIB_NIF_H_
#define EPQUEUE_C_SRC_EZLIB_NIF_H_

#include "erl_nif.h"

struct atoms
{
    ERL_NIF_TERM atomOk;
    ERL_NIF_TERM atomError;
    ERL_NIF_TERM atomTrue;
    ERL_NIF_TERM atomFalse;
    ERL_NIF_TERM atomUndefined;
    
    ERL_NIF_TERM atomGlobalLock;
};

struct epqueue_data
{
    ErlNifResourceType* resPQueueInstance;
    ErlNifResourceType* resPQueueItem;
};

extern atoms ATOMS;

#endif

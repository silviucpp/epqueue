#include "epqueue_nif.h"
#include "epqueue.h"
#include "epqueue_item.h"
#include "nif_utils.h"
#include "macros.h"

const char kAtomOk[] = "ok";
const char kAtomError[] = "error";
const char kAtomTrue[] = "true";
const char kAtomFalse[] = "false";
const char kAtomUndefined[] = "undefined";
const char kAtomGlobalLock[] = "global_lock";

atoms ATOMS;

void open_resources(ErlNifEnv* env, epqueue_data* data)
{
    ErlNifResourceFlags flags =  static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    data->resPQueueInstance = enif_open_resource_type(env, NULL, "pqueue_instance", nif_epqueue_free, flags, NULL);
    data->resPQueueItem = enif_open_resource_type(env, NULL, "pqueue_item", epqueue_item_free, flags, NULL);
}

int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(load_info);

    ATOMS.atomOk = make_atom(env, kAtomOk);
    ATOMS.atomError = make_atom(env, kAtomError);
    ATOMS.atomTrue = make_atom(env, kAtomTrue);
    ATOMS.atomFalse = make_atom(env, kAtomFalse);
    ATOMS.atomUndefined = make_atom(env, kAtomUndefined);

    ATOMS.atomGlobalLock = make_atom(env, kAtomGlobalLock);

    epqueue_data* data = static_cast<epqueue_data*>(enif_alloc(sizeof(epqueue_data)));
    open_resources(env, data);

    *priv_data = data;
    return 0;
}

void on_nif_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);

    epqueue_data* data = static_cast<epqueue_data*>(priv_data);
    enif_free(data);
}

int on_nif_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    UNUSED(old_priv);
    UNUSED(info);

    epqueue_data* data = static_cast<epqueue_data*>(enif_alloc(sizeof(epqueue_data)));
    open_resources(env, data);

    *priv = data;
    return 0;
}

static ErlNifFunc nif_funcs[] =
{
    {"new", 1, nif_epqueue_new},
    {"insert", 3, nif_epqueue_insert},
    {"remove", 2, nif_epqueue_remove},
    {"pop", 1, nif_epqueue_pop},
    {"peek", 1, nif_epqueue_peek},
    {"size", 1, nif_epqueue_size}
};

ERL_NIF_INIT(epqueue_nif, nif_funcs, on_nif_load, NULL, on_nif_upgrade, on_nif_unload)


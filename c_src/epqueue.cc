#include "epqueue.h"
#include "epqueue_item.h"
#include "epqueue_nif.h"
#include "priority_queue.h"
#include "critical_section.h"
#include "nif_utils.h"
#include "macros.h"

#include <string.h>

struct epqueue
{
    CriticalSection* crit;
    PriorityQueue* queue;
    ERL_NIF_TERM owner_pid;
};

void nif_epqueue_free(ErlNifEnv* env, void* obj)
{
    UNUSED(env);
    epqueue* qinst = static_cast<epqueue*>(obj);

    if(qinst->queue)
    {
        //clear all items
        //@todo: improve this to avoid balancing the heap when we distroy.
        while (void* item = qinst->queue->pop())
            enif_release_resource(item);

        delete qinst->queue;
    }

    if(qinst->crit)
        delete qinst->crit;
}

bool internal_insert(epqueue* q, queue_item* item)
{
    CritScope ss(q->crit);
    return q->queue->insert(item);
}

bool internal_remove(epqueue* q, queue_item* item)
{
    CritScope ss(q->crit);

    if(item->heap_index == -1)
        return false;

    return q->queue->remove(item, item->heap_index);
}

queue_item* internal_pop(epqueue* q)
{
    CritScope ss(q->crit);
    return static_cast<queue_item*>(q->queue->pop());
}

bool is_owner(ErlNifEnv* env, epqueue* q)
{
    if(q->owner_pid == 0)
        return true;

    ErlNifPid self;

    if(enif_self(env, &self) && !enif_is_identical(q->owner_pid, enif_make_pid(env, &self)))
        return false;

    return true;
}

ERL_NIF_TERM nif_epqueue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    epqueue_data* data = static_cast<epqueue_data*>(enif_priv_data(env));

    if(!enif_is_list(env, argv[0]))
        return enif_make_badarg(env);

    bool use_lock = false;

    ERL_NIF_TERM settings_list = argv[0];
    ERL_NIF_TERM head;

    while(enif_get_list_cell(env, settings_list, &head, &settings_list))
    {
        const ERL_NIF_TERM *items;
        int arity;

        if(!enif_get_tuple(env, head, &arity, &items) || arity != 2)
            return enif_make_badarg(env);

        if(enif_is_identical(items[0], ATOMS.atomGlobalLock))
            use_lock = enif_is_identical(items[1], ATOMS.atomTrue);
        else
            return enif_make_badarg(env);
    }

    epqueue* qinst = static_cast<epqueue*>(enif_alloc_resource(data->resPQueueInstance, sizeof(epqueue)));

    if(qinst == NULL)
        return make_error(env, "enif_alloc_resource failed");

    memset(qinst, 0, sizeof(epqueue));

    if(use_lock)
    {
        qinst->crit = new EnifCriticalSection();
        qinst->owner_pid = 0;
    }
    else
    {
        qinst->crit = new NullCriticalSection();
        ErlNifPid self;
        enif_self(env, &self);
        qinst->owner_pid = enif_make_pid(env, &self);
    }

    qinst->queue = new PriorityQueue(epqueue_item_less, epqueue_item_update_pos);

    ERL_NIF_TERM term = enif_make_resource(env, qinst);
    enif_release_resource(qinst);
    return enif_make_tuple2(env, ATOMS.atomOk, term);
}

ERL_NIF_TERM nif_epqueue_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    epqueue_data* data = static_cast<epqueue_data*>(enif_priv_data(env));

    epqueue* inst = NULL;

    if(!enif_get_resource(env, argv[0], data->resPQueueInstance, (void**) &inst))
        return enif_make_badarg(env);

    if(!is_owner(env, inst))
        return enif_make_badarg(env);

    CritScope ss(inst->crit);
    return enif_make_int(env, inst->queue->size());
}

ERL_NIF_TERM nif_epqueue_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    epqueue_data* data = static_cast<epqueue_data*>(enif_priv_data(env));

    epqueue* inst;
    ErlNifBinary data_bin;
    long priority;

    if(!enif_get_resource(env, argv[0], data->resPQueueInstance, (void**) &inst))
        return enif_make_badarg(env);

    if(!is_owner(env, inst))
        return enif_make_badarg(env);

    if(!enif_get_int64(env, argv[2], &priority))
        return enif_make_badarg(env);

    if(!enif_term_to_binary(env, argv[1], &data_bin))
        return enif_make_badarg(env);

    queue_item* item = epqueue_item_new(data, data_bin, priority);

    if(item == NULL)
    {
        enif_release_binary(&data_bin);
        return make_error(env, "failed to allocate a new item");
    }

    if(!internal_insert(inst, item))
    {
        enif_release_resource(&item);
        return make_error(env, "failed to insert new item in the queue");
    }

    ERL_NIF_TERM ref = enif_make_resource(env, item);
    return enif_make_tuple2(env, ATOMS.atomOk, ref);
}

ERL_NIF_TERM nif_epqueue_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    epqueue_data* data = static_cast<epqueue_data*>(enif_priv_data(env));
    epqueue* inst = NULL;
    queue_item* item = NULL;

    if(!enif_get_resource(env, argv[0], data->resPQueueInstance, (void**) &inst))
        return enif_make_badarg(env);

    if(!is_owner(env, inst))
        return enif_make_badarg(env);

    if(!enif_get_resource(env, argv[1], data->resPQueueItem, (void**) &item))
        return enif_make_badarg(env);

    if(!internal_remove(inst, item))
        return enif_make_badarg(env);

    enif_release_resource(&item);
    return ATOMS.atomOk;
}

ERL_NIF_TERM nif_epqueue_pop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    epqueue_data* data = static_cast<epqueue_data*>(enif_priv_data(env));
    epqueue* inst = NULL;

    if(!enif_get_resource(env, argv[0], data->resPQueueInstance, (void**) &inst))
        return enif_make_badarg(env);

    if(!is_owner(env, inst))
        return enif_make_badarg(env);

    queue_item* item = internal_pop(inst);

    if(item == NULL)
        return ATOMS.atomUndefined;

    ERL_NIF_TERM bin_term;

    if(enif_binary_to_term(env, item->data.data, item->data.size, &bin_term, 0) == 0)
    {
        enif_release_resource(item);
        return make_error(env, "failed to decode data");
    }

    enif_release_resource(item);
    return enif_make_tuple3(env, ATOMS.atomOk, bin_term, enif_make_int64(env, item->priority));
}

ERL_NIF_TERM nif_epqueue_peek(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    UNUSED(argc);

    epqueue_data* data = static_cast<epqueue_data*>(enif_priv_data(env));
    epqueue* inst = NULL;

    if(!enif_get_resource(env, argv[0], data->resPQueueInstance, (void**) &inst))
        return enif_make_badarg(env);

    if(!is_owner(env, inst))
        return enif_make_badarg(env);

    CritScope ss(inst->crit);
    queue_item* item = static_cast<queue_item*>(inst->queue->peek());

    if(item == NULL)
        return ATOMS.atomUndefined;

    ERL_NIF_TERM bin_term;

    if(enif_binary_to_term(env, item->data.data, item->data.size, &bin_term, 0) == 0)
        return make_error(env, "failed to decode data");

    return enif_make_tuple3(env, ATOMS.atomOk, bin_term, enif_make_int64(env, item->priority));
}

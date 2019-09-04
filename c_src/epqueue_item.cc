#include "epqueue_item.h"
#include "epqueue_nif.h"
#include "macros.h"

bool epqueue_item_less(void* ax, void* bx)
{
    queue_item* a = static_cast<queue_item*>(ax);
    queue_item* b = static_cast<queue_item*>(bx);
    return a->priority < b->priority;
}

void epqueue_item_update_pos(void* ax, int32_t pos)
{
    queue_item* a = static_cast<queue_item*>(ax);
    a->heap_index = pos;
}

void epqueue_item_free(ErlNifEnv* env, void* obj)
{
    UNUSED(env);
    queue_item* item = static_cast<queue_item*>(obj);
    enif_release_binary(&item->data);
}

queue_item* epqueue_item_new(const epqueue_data* data, const ErlNifBinary& bin, uint64_t priority)
{
    queue_item* item = static_cast<queue_item*>(enif_alloc_resource(data->resPQueueItem, sizeof(queue_item)));

    if(item == NULL)
        return NULL;

    item->heap_index = -1;
    item->priority = priority;
    item->data = bin;
    return item;
}

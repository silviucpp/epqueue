#ifndef C_SRC_EPQUEUE_ITEM_H_
#define C_SRC_EPQUEUE_ITEM_H_

#include "erl_nif.h"
#include <stdint.h>

struct epqueue_data;

struct queue_item
{
    int32_t heap_index;
    uint64_t priority;
    uint64_t internal_id;
    ErlNifBinary data;
};

void epqueue_item_update_pos(void* ax, int32_t pos);
bool epqueue_item_less(void* ax, void* bx);

queue_item* epqueue_item_new(const epqueue_data* data, const ErlNifBinary& bin, uint64_t priority);
void epqueue_item_free(ErlNifEnv* env, void* obj);

#endif  // C_SRC_EPQUEUE_ITEM_H_

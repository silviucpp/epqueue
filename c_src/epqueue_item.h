#ifndef EPQUEUE_C_SRC_PRIORITY_EPQUEUE_ITEM_H_
#define EPQUEUE_C_SRC_PRIORITY_EPQUEUE_ITEM_H_

#include "erl_nif.h"

struct epqueue_data;

struct queue_item
{
    int heap_index;
    long priority;
    ErlNifBinary data;
};

void epqueue_item_update_pos(void* ax, int pos);
bool epqueue_item_less(void* ax, void* bx);

queue_item* epqueue_item_new(const epqueue_data* data, const ErlNifBinary& bin, long priority);
void epqueue_item_free(ErlNifEnv* env, void* obj);

#endif

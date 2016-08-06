#include "priority_queue.h"
#include "erl_nif.h"
#include <stdlib.h>
#include <string.h>

//Algorithm described at : http://robin-thomas.github.io/min-heap/

#define left(x) 2 * x + 1
#define right(x) 2 * x + 2
#define parent(x) (x - 1) / 2

#define less(a, b) less_(heap_[a], heap_[b])

PriorityQueue::PriorityQueue(LessFun ls, UpdatePositionFun upd, DestroyElementFun dtor) :
    capacity_(0),
    length_(0),
    heap_(NULL),
    less_(ls),
    update_pos_fun_(upd),
    item_dtor_(dtor) { }

PriorityQueue::~PriorityQueue()
{
    if(heap_)
    {
        for(int i = 0; i < length_; i++)
            item_dtor_(heap_[i]);

        enif_free(heap_);
    }
}

bool PriorityQueue::insert(void* item)
{
    int pos;

    if (length_ == capacity_)
    {
        int new_capacity = (length_+1) * 2;
        void** new_heap = (void**) enif_alloc(sizeof(void*) * new_capacity);

        if (!new_heap)
            return false;

        memcpy(new_heap, heap_, sizeof(void*)*length_);
        enif_free(heap_);
        heap_ = new_heap;
        capacity_ = new_capacity;
    }

    pos = (length_)++;
    set(pos, item);
    bubble_down(pos);
    return true;
}

bool PriorityQueue::remove(void* item, int pos)
{
    if (pos >= length_)
        return false;

    if(heap_[pos] != item)
        return false;

    return remove(pos) == item;
}

void* PriorityQueue::remove(int pos)
{
    if (pos >= length_)
        return NULL;

    void* item = heap_[pos];
    length_--;

    int ls = less(pos, length_);

    set(pos, heap_[length_]);

    if(ls)
        bubble_up(pos);
    else
        bubble_down(pos);

    //@todo: resize down the heap in case we have a lot of empty slots

    update_pos_fun_(item, -1);
    return item;
}

void* PriorityQueue::peek()
{
    if(length_ == 0)
        return NULL;

    return heap_[0];
}

//private

void PriorityQueue::set(int pos, void* item)
{
    heap_[pos] = item;
    update_pos_fun_(item, pos);
}

void PriorityQueue::swap(int pos1, int pos2)
{
    void* tmp = heap_[pos1];
    set(pos1, heap_[pos2]);
    set(pos2, tmp);
}

void PriorityQueue::bubble_down(int pos)
{
    while(true)
    {
        int parent = parent(pos);

        if (pos == 0 || less(parent, pos))
            return;

        swap(pos, parent);
        pos = parent;
    }
}

void PriorityQueue::bubble_up(int pos)
{
    while (true)
    {
        int left = left(pos);
        int right = right(pos);
        int smallest = pos;

        if (left < length_ && less(left, smallest))
            smallest = left;

        if (right < length_ && less(right, smallest))
            smallest = right;

        if (smallest == pos)
            return;

        swap(pos, smallest);
        pos = smallest;
    }
}

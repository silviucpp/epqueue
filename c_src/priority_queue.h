#ifndef C_SRC_PRIORITY_QUEUE_H_
#define C_SRC_PRIORITY_QUEUE_H_

#include "macros.h"

typedef bool(*LessFun)(void*, void*);
typedef void(*UpdatePositionFun)(void*, int);
typedef void(*DestroyElementFun)(void*);

class PriorityQueue
{
public:

    PriorityQueue(LessFun ls, UpdatePositionFun upd, DestroyElementFun dtor);
    ~PriorityQueue();

    bool insert(void* item);
    bool remove(void* item, int pos);
    void* remove(int pos);
    void* pop() {return remove(0);}
    void* peek();
    int size() const { return length_;}
    int rank(int pos) {return rank_r(pos, 0)+1;}

private:

    inline void set(int pos, void* item);
    inline void pos_swap(int pos1, int pos2);
    void bubble_down(int pos);
    void bubble_up(int pos);
    int rank_r(int x, int cur_node_index);

    int capacity_;
    int length_;
    void** heap_;
    LessFun less_;
    UpdatePositionFun update_pos_fun_;
    DestroyElementFun item_dtor_;

    DISALLOW_COPY_AND_ASSIGN(PriorityQueue);
};

#endif  // C_SRC_PRIORITY_QUEUE_H_

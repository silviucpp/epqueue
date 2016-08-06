#ifndef EPQUEUE_C_SRC_PRIORITY_QUEUE_H_
#define EPQUEUE_C_SRC_PRIORITY_QUEUE_H_

#include "macros.h"

typedef int(*LessFun)(void*, void*);
typedef void(*UpdatePositionFun)(void*, int);

class PriorityQueue
{
public:
    
    PriorityQueue(LessFun ls, UpdatePositionFun upd);
    ~PriorityQueue();
    
    int size() const { return length_;}
    bool insert (void* item);
    bool remove(void* item, int pos);
    void* remove(int pos);
    void* pop() {return remove(0);}
    void* peek();
    
private:
    
    DISALLOW_COPY_AND_ASSIGN(PriorityQueue);
    
    inline void set(int pos, void* item);
    inline void swap(int pos1, int pos2);
    void bubble_down(int pos);
    void bubble_up(int pos);
    
    int capacity_;
    int length_;
    void** heap_;
    LessFun less_;
    UpdatePositionFun update_pos_fun_;
};

#endif

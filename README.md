epqueue
================

A high performant Erlang Priority Queue implemented using a binary heap (using NIF)

Concurrency
-----------

The queues provides support for concurrent access. You can specify the concurrent model using `{global_lock, true|false}` option (default is `false`).

Based on `global_lock` option the queue behaves as follow:

- `false` : The process that creates the queue is the only one that can operate on it. If any other process will try to access the queue will fail with a `badarg` exception
- `true` : The returned queue reference can be stored and passed between processes on the same node. Also any process can operate with the queue (a mutex will be used for concurrency control)

Lifetime
-----------

Like any other data a queue reference returned using `epqueue:new` or an item reference returned using `epqueue:insert` are garbaged collected by Erlang VM when there is no longer any reference to them.
The references can be stored and passed between processes on the same node and also sored in a ETS table.

Quick start
-----------

You need to have at least Erlang OTP 19. This is because `enif_binary_to_term` and `enif_term_to_binary` are not available in previous versions.
This can be changed if it's required by moving the encoding/decoding or terms in Erlang and in NIF only store the received binary.

Compile:

```sh
rebar compile
```

or

```sh
rebar3 compile
```

Simple usage:

```erlang
{ok, Q} = epqueue:new([]).
{ok, _Ref} = epqueue:insert(Q, 1, 1).
{ok, 1, 1} = epqueue:peek(Q).
1 = epqueue:size(Q).
{ok, 1, 1} = epqueue:pop(Q).
0 = epqueue:size(Q).
```

API
-----------

- `epqueue:new(Opts)` - Create a new queue. As argument receives a list of settings. The only supported option right now is `{global_lock, true|false}`. For more details read the Concurrency section.
- `insert(QueueRef, Data, Priority)` - Insert an element in the queue with a specified priority. In case of success returns `{ok, ItemRef}`. The reference can be used later in case you want to remove a specific element form the queue.  
- `remove(QueueRef, Ref)` - Remove a specified element from the queue. `Ref` is the one returned by the `epqueue:insert/3` method.
- `pop(QueueRef)` - Removes the element with the lowest priority from the queue (the head element). Returns `{ok, Data, Priority}` in case of success
- `peek(QueueRef)` - Returns the element `{ok, Data, Priority}` with the lowest priority (the head element) without removing it.
- `epqueue:size(QueueRef)` - Get the size of a queue.

Tests
------------

In order to run the integrity tests run `make ct` from project root. 

Performance testing
-----------

Results are generated on a MacBook Pro (Intel Core i7 4 cores at 2.5 GHz):
The `insert overhead` is the time spent to generate `ELEMENTS` random numbers for the priorities,

- `make bench_serial` inserts a number of `ELEMENTS` with priorities from 0 to `MAX_PRIORITY` in a queue that
use or not a lock.

```erl
make bench_serial ELEMENTS=1000000 MAX_PRIORITY=10000000 USE_LOCK=true
insert overhead: 252.764 ms insert time: 740.722 ms pop time: 1833.721 ms

make bench_serial ELEMENTS=1000000 MAX_PRIORITY=10000000 USE_LOCK=false
insert overhead: 250.178 ms insert time: 726.999 ms pop time: 1771.064 ms
```

- `bench_concurrent` spawn a number of `PROCS` processes that will insert a number of `ELEMENTS` with priorities 
from 0 to `MAX_PRIORITY` in a queue (lock is mandatory).

```erl
make bench_concurrent PROCS=1 ELEMENTS=1000000 MAX_PRIORITY=10000000
insert overhead: 274.339 ms insert time: 778.185 ms pop time: 1772.712 ms

make bench_concurrent PROCS=2 ELEMENTS=1000000 MAX_PRIORITY=10000000
insert overhead: 139.748 ms insert time: 2408.561 ms pop time: 4563.286 ms 

make bench_concurrent PROCS=3 ELEMENTS=1000000 MAX_PRIORITY=10000000
insert overhead: 100.252 ms insert time: 3528.367 ms pop time: 4913.981 ms 

make bench_concurrent PROCS=4 ELEMENTS=1000000 MAX_PRIORITY=10000000
insert overhead: 77.775 ms insert time: 3603.385 ms pop time: 5055.776 ms 

make bench_concurrent PROCS=20 ELEMENTS=1000000 MAX_PRIORITY=10000000
insert overhead: 76.704 ms insert time: 3676.474 ms pop time: 5039.594 ms 
```

-module(epqueue).
-author("silviu.caragea").

-export([new/0, new/1, size/1, insert/3, remove/2, pop/1, peek/1]).

new() ->
    epqueue_nif:new([]).

new(Options) ->
    epqueue_nif:new(Options).

size(QueueRef) ->
    epqueue_nif:size(QueueRef).

insert(QueueRef, Data, Priority) ->
    epqueue_nif:insert(QueueRef, Data, Priority).

remove(QueueRef, Ref) ->
    epqueue_nif:remove(QueueRef, Ref).

pop(QueueRef) ->
    epqueue_nif:pop(QueueRef).

peek(QueueRef) ->
    epqueue_nif:peek(QueueRef).
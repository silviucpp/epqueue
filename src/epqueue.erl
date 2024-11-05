-module(epqueue).

-export([
    new/0,
    new/1,
    size/1,
    insert/3,
    remove/2,
    pop/1,
    peek/1,
    rank/2
]).

-type error() :: badarg | {error, binary()}.
-type queue_option() :: {global_lock, boolean()}.
-type priority() :: non_neg_integer().
-type data() :: any().
-type queue_ref() :: reference().
-type data_ref() :: reference().

-spec new() ->
    {ok, queue_ref()} | error().

new() ->
    epqueue_nif:new([]).

-spec new([queue_option()]) ->
    {ok, queue_ref()} | error().

new(Options) ->
    epqueue_nif:new(Options).

-spec size(queue_ref()) ->
    non_neg_integer() | badarg.

size(QueueRef) ->
    epqueue_nif:size(QueueRef).

-spec insert(queue_ref(), data(), priority()) ->
    {ok, data_ref()} | error().

insert(QueueRef, Data, Priority) ->
    epqueue_nif:insert(QueueRef, Data, Priority).

-spec remove(queue_ref(), data_ref()) ->
    boolean() | error().

remove(QueueRef, Ref) ->
    epqueue_nif:remove(QueueRef, Ref).

-spec pop(queue_ref()) ->
    {ok, data(), priority()} | error().

pop(QueueRef) ->
    epqueue_nif:pop(QueueRef).

-spec peek(queue_ref()) ->
    {ok, data(), priority()} | error().

peek(QueueRef) ->
    epqueue_nif:peek(QueueRef).

-spec rank(queue_ref(), data_ref()) ->
    {ok, non_neg_integer()} | error().

rank(QueueRef, Ref) ->
    epqueue_nif:rank(QueueRef, Ref).

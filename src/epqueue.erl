-module(epqueue).
-author("silviu.caragea").

-export([new/0, new/1, size/1, insert/3, remove/2, pop/1, peek/1]).


-spec(new() -> {ok, QueueRef::reference()} | badarg | {error, Reason :: binary()}).

new() ->
    epqueue_nif:new([]).

-spec(new(Options::list()) -> {ok, QueueRef :: reference()} | badarg | {error, Reason :: binary()}).

new(Options) ->
    epqueue_nif:new(Options).

-spec(size(QueueRef::reference()) -> badarg | integer()).

size(QueueRef) ->
    epqueue_nif:size(QueueRef).

-spec(insert(QueueRef::reference(), Data::term(), Priority::integer()) ->
    {ok, ItemRef :: reference()} | badarg | {error, Reason :: binary()}).

insert(QueueRef, Data, Priority) ->
    epqueue_nif:insert(QueueRef, Data, Priority).

-spec(remove(QueueRef::reference(), Ref::reference()) ->
    {ok, Data::term(), Priority::integer()} | badarg | {error, Reason :: binary()}).

remove(QueueRef, Ref) ->
    epqueue_nif:remove(QueueRef, Ref).

-spec(pop(QueueRef::reference()) ->
    {ok, Data::term(), Priority::integer()} | badarg | {error, Reason :: binary()}).

pop(QueueRef) ->
    epqueue_nif:pop(QueueRef).

-spec(peek(QueueRef::reference()) ->
    {ok, Data::term(), Priority::integer()} | badarg | {error, Reason :: binary()}).

peek(QueueRef) ->
    epqueue_nif:peek(QueueRef).
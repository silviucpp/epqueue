-module(test).
-author("silviu.caragea").

-export([basic_ops/0, empty_queue/0, non_empty_queue/0, test_remove/0, test_pop/0]).

basic_ops() ->
    {ok, Q} = epqueue:new([]),
    {ok, _Ref} = epqueue:insert(Q, 1, 1),
    {ok, 1, 1} = epqueue:peek(Q),
    1 = epqueue:size(Q),
    {ok, 1, 1} = epqueue:pop(Q),
    0 = epqueue:size(Q),
    ok.

empty_queue() ->
    {ok, _} = epqueue:new([]),
    ok.

non_empty_queue() ->
    {ok, Q1} = epqueue:new([]),
    {ok, _} = epqueue:insert(Q1, 1, 1),
    {ok, _} = epqueue:insert(Q1, 2, 2),
    {ok, _} = epqueue:insert(Q1, 3, 3),
    {ok, _} = epqueue:insert(Q1, 4, 4),
    {ok, _} = epqueue:insert(Q1, 5, 5),
    ok.

test_remove() ->
    {ok, Q1} = epqueue:new([]),
    {ok, Q2} = epqueue:new([]),
    {ok, Ref7} = epqueue:insert(Q2, 7, 7),
    {ok, Ref3} = epqueue:insert(Q1, 3, 3),
    {ok, Ref5} = epqueue:insert(Q1, 5, 5),
    {ok, Ref1} = epqueue:insert(Q1, 1, 1),
    {error, _} = epqueue:remove(Q1, Ref7),
    ok = epqueue:remove(Q1, Ref5),
    ok = epqueue:remove(Q1, Ref3),
    ok = epqueue:remove(Q1, Ref1),
    ok = epqueue:remove(Q2, Ref7).

test_pop() ->
    {ok, Q1} = epqueue:new([]),
    epqueue:insert(Q1, 3, 3),
    epqueue:insert(Q1, 5, 5),
    epqueue:insert(Q1, 1, 1),
    {ok, 1, 1} = epqueue:pop(Q1),
    {ok, 3, 3} = epqueue:pop(Q1),
    {ok, 5, 5} = epqueue:pop(Q1),
    ok.
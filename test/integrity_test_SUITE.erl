-module(integrity_test_SUITE).

-compile(export_all).

all() -> [
    {group, epqueue_group}
].

groups() -> [
    {epqueue_group, [sequence], [
        basic_ops,
        empty_queue,
        non_empty_queue,
        test_remove,
        test_pop
    ]}
].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

basic_ops(_Config) ->
    {ok, Q} = epqueue:new([]),
    {ok, _Ref} = epqueue:insert(Q, 1, 1),
    {ok, 1, 1} = epqueue:peek(Q),
    1 = epqueue:size(Q),
    {ok, 1, 1} = epqueue:pop(Q),
    0 = epqueue:size(Q),
    ok.

empty_queue(_Config) ->
    {ok, _} = epqueue:new([]),
    ok.

non_empty_queue(_Config) ->
    {ok, Q1} = epqueue:new([]),
    {ok, _} = epqueue:insert(Q1, 1, 1),
    {ok, _} = epqueue:insert(Q1, 2, 2),
    {ok, _} = epqueue:insert(Q1, 3, 3),
    {ok, _} = epqueue:insert(Q1, 4, 4),
    {ok, _} = epqueue:insert(Q1, 5, 5),
    ok.

test_remove(_Config) ->
    {ok, Q1} = epqueue:new([]),
    {ok, Q2} = epqueue:new([]),
    {ok, Ref7} = epqueue:insert(Q2, 7, 7),
    {ok, Ref3} = epqueue:insert(Q1, 3, 3),
    {ok, Ref5} = epqueue:insert(Q1, 5, 5),
    {ok, Ref1} = epqueue:insert(Q1, 1, 1),
    false = epqueue:remove(Q1, Ref7),
    true = epqueue:remove(Q1, Ref5),
    true = epqueue:remove(Q1, Ref3),
    true = epqueue:remove(Q1, Ref1),
    true = epqueue:remove(Q2, Ref7),
    ok.

test_pop(_Config) ->
    {ok, Q1} = epqueue:new([]),
    epqueue:insert(Q1, 3, 3),
    epqueue:insert(Q1, 5, 5),
    epqueue:insert(Q1, 1, 1),
    {ok, 1, 1} = epqueue:pop(Q1),
    {ok, 3, 3} = epqueue:pop(Q1),
    {ok, 5, 5} = epqueue:pop(Q1),
    ok.
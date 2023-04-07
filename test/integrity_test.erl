-module(integrity_test).

-include_lib("eunit/include/eunit.hrl").

basic_ops_test() ->
    {ok, Q} = epqueue:new([]),
    {ok, _Ref} = epqueue:insert(Q, 1, 1),
    {ok, 1, 1} = epqueue:peek(Q),
    ?assertEqual(1, epqueue:size(Q)),
    ?assertEqual({ok, 1, 1}, epqueue:pop(Q)),
    ?assertEqual(0, epqueue:size(Q)),
    ok.

insert_remove_test() ->
    {ok, Q1} = epqueue:new([]),
    {ok, R5} = epqueue:insert(Q1, 5, 2),
    {ok, R1} = epqueue:insert(Q1, 1, 0),
    {ok, R3} = epqueue:insert(Q1, 3, 1),
    {ok, R2} = epqueue:insert(Q1, 2, 0),
    {ok, R4} = epqueue:insert(Q1, 4, 1),
    ?assertEqual(5, epqueue:size(Q1)),
    ?assertEqual(true, epqueue:remove(Q1, R1)),
    ?assertEqual(true, epqueue:remove(Q1, R2)),
    ?assertEqual(true, epqueue:remove(Q1, R3)),
    ?assertEqual(true, epqueue:remove(Q1, R4)),
    ?assertEqual(true, epqueue:remove(Q1, R5)),
    ?assertEqual(0, epqueue:size(Q1)),
    ok.

remove_test() ->
    {ok, Q1} = epqueue:new([]),
    {ok, Q2} = epqueue:new([]),
    {ok, Ref7} = epqueue:insert(Q2, 7, 7),
    {ok, Ref3} = epqueue:insert(Q1, 3, 3),
    {ok, Ref5} = epqueue:insert(Q1, 5, 5),
    {ok, Ref1} = epqueue:insert(Q1, 1, 1),
    ?assertEqual(false, epqueue:remove(Q1, Ref7)),
    ?assertEqual(true, epqueue:remove(Q1, Ref5)),
    ?assertEqual(true, epqueue:remove(Q1, Ref3)),
    ?assertEqual(true, epqueue:remove(Q1, Ref1)),
    ?assertEqual(true, epqueue:remove(Q2, Ref7)),
    ok.

remove_equal_prio_test() ->
    {ok, Q1} = epqueue:new([]),
    {ok, Ref1} = epqueue:insert(Q1, 1, 1),
    {ok, Ref2} = epqueue:insert(Q1, 2, 1),
    ?assertEqual(true, epqueue:remove(Q1, Ref2)),
    ?assertEqual(true, epqueue:remove(Q1, Ref1)),
    ok.

pop_test() ->
    {ok, Q1} = epqueue:new([]),
    ?assertMatch({ok, _}, epqueue:insert(Q1, 3, 3)),
    ?assertMatch({ok, _}, epqueue:insert(Q1, 5, 5)),
    ?assertMatch({ok, _}, epqueue:insert(Q1, 1, 1)),
    ?assertEqual({ok, 1, 1}, epqueue:pop(Q1)),
    ?assertEqual({ok, 3, 3}, epqueue:pop(Q1)),
    ?assertEqual({ok, 5, 5}, epqueue:pop(Q1)),
    ok.

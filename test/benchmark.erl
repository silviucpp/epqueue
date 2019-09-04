-module(benchmark).

-export([
    benchmark_serial/3,
    benchmark_concurrent/3
]).

benchmark_serial(Elements, MaxPriority, Lock) ->
    rand:uniform(), %just to init the seed
    {ok, Q} = epqueue:new([{global_lock, Lock}]),

    {T0, ok} = timer:tc(fun() -> insert_none(Elements, MaxPriority) end),
    {T1, ok} = timer:tc(fun() -> insert_item(Elements, Q, MaxPriority) end),
    {T2, ok} = timer:tc(fun() -> remove_item(Q) end),

    T0Ms = T0/1000,
    T1Ms = T1/1000,
    T2Ms = T2/1000,

    io:format(<<"insert overhead: ~p ms insert time: ~p ms pop time: ~p ms ~n">>, [T0Ms, T1Ms, T2Ms]).

benchmark_concurrent(Procs, Elements, MaxPriority) ->
    {ok, Q} = epqueue:new([{global_lock, true}]),

    ElsPerProcess = round(Elements/Procs),

    InsertNoneWorkFun = fun() ->
        insert_none(ElsPerProcess, MaxPriority)
    end,

    InsertWorkFun = fun() ->
        insert_item(ElsPerProcess, Q, MaxPriority)
    end,

    RemoveWorkFun = fun() ->
        remove_item(Q)
    end,

    {T0, _} = timer:tc(fun()-> multi_spawn:do_work(InsertNoneWorkFun, Procs) end),
    {T1, _} = timer:tc(fun()-> multi_spawn:do_work(InsertWorkFun, Procs) end),
    {T2, _} = timer:tc(fun()-> multi_spawn:do_work(RemoveWorkFun, Procs) end),

    T0Ms = T0/1000,
    T1Ms = T1/1000,
    T2Ms = T2/1000,

    io:format(<<"insert overhead: ~p ms insert time: ~p ms pop time: ~p ms ~n">>, [T0Ms, T1Ms, T2Ms]).

insert_item(0, _Q, _Max) ->
    ok;
insert_item(N, Q, Max) ->
    El = rand:uniform(Max),
    {ok, _} = epqueue:insert(Q, El, El),
    insert_item(N-1, Q, Max).

remove_item(Q) ->
    case epqueue:pop(Q) of
        undefined->
            ok;
        {ok, _, _} ->
            remove_item(Q)
    end.

insert_none(0, _Max) ->
    ok;
insert_none(N, Max) ->
    rand:uniform(Max),
    insert_none(N-1, Max).
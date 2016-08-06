-module(epqueue_nif).
-author("silviu.caragea").

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(load_nif/0).

-export([
    new/1,
    size/1,
    insert/3,
    remove/2,
    pop/1,
    peek/1
]).

%% nif functions

load_nif() ->
    SoName = get_nif_library_path(),
    io:format(<<"Loading library: ~p ~n">>, [SoName]),
    ok = erlang:load_nif(SoName, 0).

get_nif_library_path() ->
    case code:priv_dir(ezlib) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                false ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

new(_Options) ->
    ?NOT_LOADED.

size(_QueueRef) ->
    ?NOT_LOADED.

insert(_QueueRef, _Data, _Priority) ->
    ?NOT_LOADED.

remove(_QueueRef, _Ref) ->
    ?NOT_LOADED.

pop(_QueueRef) ->
    ?NOT_LOADED.

peek(_QueueRef) ->
    ?NOT_LOADED.
-module(ephp_cover).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    start_link/0,
    store/2,
    store/3,
    dump/0,
    get_config/0
]).

-record(cover, {
    timestamp = os:timestamp(),
    file,
    line
}).

start_link() ->
    case erlang:get(cover) of
        undefined ->
            erlang:put(cover, []),
            ok;
        _ ->
            ok
    end.

store(_Cover, _FileOrContext, undefined) ->
    ok;
store(true = _Cover, FileOrContext, Line) ->
    store(FileOrContext, Line);
store(false = _Cover, _FileOrContext, _Line) ->
    ok.

store(File, {_,L,C}) ->
    store(File, {{line,L},{column,C}});
store(_File, undefined) ->
    ok;
store(File, Line) when is_binary(File) ->
    Cover = erlang:get(cover),
    Cover = erlang:put(cover, [#cover{file = File, line = Line}|Cover]),
    ok;
store(Context, Line) when is_reference(Context) ->
    File = ephp_context:get_active_file(Context),
    store(File, Line).

dump() ->
    Cover = erlang:get(cover),
    dump(lists:reverse(Cover)).

dump([]) ->
    ok;
dump([#cover{file = File,
             line = {{line, L}, {column, C}},
             timestamp = TS} | Cover]) ->
    io:format("~b \"~s\" ~p,~p~n", [epoch(TS), File, L, C]),
    dump(Cover).

get_config() ->
    case ephp_data:to_bool(ephp_config:get(<<"cover">>)) of
        true -> true;
        _ -> false
    end.

epoch({A,B,C}) ->
    (A * 1000000 + B) * 1000000 + C.

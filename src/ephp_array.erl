-module(ephp_array).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    new/0,
    size/1,
    find/2,
    store/3,
    erase/2,
    fold/3,
    to_list/1
]).

new() -> #ephp_array{}.

size(#ephp_array{size=Size}) -> Size.

find(Key, #ephp_array{values=Values}) ->
    case lists:keyfind(Key, 1, Values) of
        {Key, Value} -> {ok, Value};
        false -> error
    end.

store(auto, Value, #ephp_array{last_num_index=Key, values=Values}=Array) ->
    Array#ephp_array{
        last_num_index = Key + 1,
        values = Values ++ [{Key, Value}],
        size = Array#ephp_array.size + 1
    };

store(Key, Value, #ephp_array{last_num_index=Last, values=Values}=Array) when
        is_integer(Key) andalso Key >= 0 andalso
        Last =< Key ->
    Array#ephp_array{
        last_num_index = Key,
        values = Values ++ [{Key, Value}],
        size = Array#ephp_array.size + 1
    };

store(Key, Value, #ephp_array{values=Values}=Array) ->
    case lists:keyfind(Key, 1, Values) =/= false of
        true ->
            NewValues = lists:keyreplace(Key, 1, Values, {Key, Value}),
            Size = Array#ephp_array.size;
        false ->
            NewValues = Values ++ [{Key, Value}],
            Size = Array#ephp_array.size + 1
    end,
    Array#ephp_array{
        values = NewValues,
        size = Size
    }.

erase(Key, #ephp_array{values=Values}=Array) ->
    NewValues = lists:keydelete(Key, 1, Values),
    Array#ephp_array{
        values = NewValues,
        size = length(NewValues)
    }.

fold(Fun, Initial, #ephp_array{values=Values}) ->
    lists:foldl(fun({K,V}, Acc) -> Fun(K, V, Acc) end, Initial, Values).

to_list(#ephp_array{values=Values}) -> Values.

-module(ephp_array).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    new/0,
    new/3,
    size/1,
    find/2,
    store/3,
    erase/2,
    fold/3,
    from_list/1,
    to_list/1
]).

new() -> #ephp_array{}.

new(Module, Function, Args) ->
    #ephp_array{trigger = {Module, Function, Args}}.

size(#ephp_array{size=Size}) -> Size.

find(Key, #ephp_array{values=Values, trigger=undefined}) ->
    case lists:keyfind(Key, 1, Values) of
        {Key, Value} -> {ok, Value};
        false -> error
    end;

find(Key, #ephp_array{trigger={Module,Function,Args}}=Array) ->
    apply(Module, Function, Args ++ [Array, {retrieve, Key}]).

store(auto, Value, #ephp_array{last_num_index=Key, values=Values}=Array) ->
    report(Array#ephp_array{
        last_num_index = Key + 1,
        values = Values ++ [{Key, Value}],
        size = Array#ephp_array.size + 1
    }, {add, Key, Value});

store(Key, Value, #ephp_array{last_num_index=Last, values=Values}=Array) when
        is_integer(Key) andalso Key >= 0 andalso
        Last =< Key ->
    report(Array#ephp_array{
        last_num_index = Key,
        values = Values ++ [{Key, Value}],
        size = Array#ephp_array.size + 1
    }, {add, Key, Value});

store(Key, Value, #ephp_array{values=Values}=Array) ->
    case lists:keyfind(Key, 1, Values) =/= false of
        true ->
            NewValues = lists:keyreplace(Key, 1, Values, {Key, Value}),
            Action = update,
            Size = Array#ephp_array.size;
        false ->
            NewValues = Values ++ [{Key, Value}],
            Action = add,
            Size = Array#ephp_array.size + 1
    end,
    report(Array#ephp_array{
        values = NewValues,
        size = Size
    }, {Action, Key, Value}).

erase(Key, #ephp_array{values=Values}=Array) ->
    NewValues = lists:keydelete(Key, 1, Values),
    report(Array#ephp_array{
        values = NewValues,
        size = length(NewValues)
    }, {remove, Key}).

fold(Fun, Initial, #ephp_array{values=Values, trigger=undefined}) ->
    lists:foldl(fun({K,V}, Acc) -> Fun(K, V, Acc) end, Initial, Values);

fold(Fun, Initial, #ephp_array{trigger={Module,Function,Args}}=Array) ->
    NewFun = fun({K,V},Acc) -> Fun(K,V,Acc) end,
    apply(Module, Function, Args ++ [Array, {fold, NewFun, Initial}]).

from_list(List) when is_list(List) ->
    lists:foldl(fun({K,_}=E, #ephp_array{values=V, size=S}=A)
            when is_binary(K) orelse is_number(K) ->
        A#ephp_array{size = S + 1, values = V ++ [E]}
    end, #ephp_array{}, List).

to_list(#ephp_array{values=Values, trigger=undefined}) ->
    Values;

to_list(#ephp_array{trigger={Module,Function,Args}}=Array) ->
    apply(Module, Function, Args ++ [Array, to_list]).

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------

report(#ephp_array{trigger=undefined}=Array, _) ->
    Array;

report(#ephp_array{trigger={Module,Function,Args}}=Array, Action) ->
    apply(Module, Function, Args ++ [Array, Action]).

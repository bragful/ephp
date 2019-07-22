%% @doc Array related functions. Here you can see documentation about the
%%      implementation of the PHP functions for the section called <i>Array</i>.
-module(ephp_lib_array).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    handle_error/3,
    in_array/5,
    count/3,
    array_merge/3,
    list/3,
    array_unique/4,
    array_change_key_case/4,
    array_chunk/5,
    array_column/5,
    array_pop/3,
    reset/3,
    current/3,
    php_end/3,
    prev/3,
    next/3,
    key/3,
    ksort/4,
    array_keys/3
]).

-include("ephp.hrl").

-define(CASE_LOWER, 0).
-define(CASE_UPPER, 1).

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {in_array, [{args, {2, 3, undefined, [mixed, array, {boolean, false}]}}]},
    count,
    {count, [{alias, <<"sizeof">>}]},
    {array_merge, [pack_args]},
    {list, [pack_args, {args, no_resolve}]},
    {array_unique, [
        {args, {1, 2, undefined, [array, {integer, ?SORT_STRING}]}}
    ]},
    {array_change_key_case, [
        {args, {1, 2, undefined, [array, {integer, ?CASE_LOWER}]}}
    ]},
    {array_chunk, [
        {args, {2, 3, undefined, [array, integer, {boolean, false}]}}
    ]},
    {array_column, [
        {args, {2, 3, undefined, [array, mixed, {mixed, undefined}]}}
    ]},
    {reset, [{args, {1, 1, undefined, [array]}}]},
    {current, [{args, {1, 1, undefined, [array]}}]},
    {current, [{args, {1, 1, undefined, [array]}}, {alias, <<"pos">>}]},
    {php_end, [{args, {1, 1, undefined, [array]}}, {alias, <<"end">>}]},
    {prev, [{args, {1, 1, undefined, [array]}}]},
    {next, [{args, {1, 1, undefined, [array]}}]},
    {next, [{args, {1, 1, undefined, [array]}}, {alias, <<"each">>}]},
    {key, [{args, {1, 1, undefined, [array]}}]},
    {ksort, [{args, {1, 2, false, [array, {integer, ?SORT_REGULAR}]}}]},
    {array_keys, [array]},
    {array_pop, [array]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().
%% @private
init_const() -> [
    {<<"SORT_REGULAR">>, ?SORT_REGULAR},
    {<<"SORT_NUMERIC">>, ?SORT_NUMERIC},
    {<<"SORT_STRING">>, ?SORT_STRING},
    {<<"SORT_LOCALE_STRING">>, ?SORT_LOCALE_STRING},
    {<<"CASE_LOWER">>, ?CASE_LOWER},
    {<<"CASE_UPPER">>, ?CASE_UPPER}
].

-spec handle_error(ephp_error:error_type(), ephp_error:error_level(),
                   Args::term()) -> string() | ignore.
%% @private
handle_error(enooffset, _Level, {Offset}) ->
    io_lib:format("Undefined offset: ~p", [Offset]);

handle_error(_Type, _Level, _Data) ->
    ignore.

-spec in_array(
    context(), line(),
    Key :: var_value(), Array :: var_value(), Strict :: var_value()
) -> boolean().

in_array(_Context, _Line, {_,Value}, {_,Array}, {_,Strict}) ->
    member(Value, Array, ephp_data:to_bool(Strict)).

-spec count(context(), line(), Array :: var_value()) -> integer().

count(_Context, _Line, {_,Array}) when ?IS_ARRAY(Array) ->
    ephp_array:size(Array);

count(_Context, _Line, _Var) ->
    1.

-spec array_merge(context(), line(), Arrays :: [var_value()]) -> ephp_array().

array_merge(Context, Line, Args) ->
    array_merge(Context, Line, 1, Args).

-spec list(context(), line(), Vars :: [var_value()]) -> ephp_array() | undefined.

list(Context, Line, [{#ephp_array{}=Array, undefined}|Getters]) ->
    lists:foldl(fun
        ({[], _}, I) ->
            I + 1;
        ({Getter, _}, I) ->
            case ephp_array:find(I, Array) of
                {ok, Value} ->
                    ephp_context:set(Context, Getter, Value);
                error ->
                    File = ephp_context:get_active_file(Context),
                    Error = {error, enooffset, Line, File, ?E_NOTICE, {I}},
                    ephp_error:handle_error(Context, Error),
                    ephp_context:set(Context, Getter, undefined)
            end,
            I + 1
    end, 0, Getters),
    Array;

list(Context, _Line, [{Binary, undefined}|Getters]) when is_binary(Binary) ->
    lists:foreach(fun({[], _}) -> ok;
                     ({G, _}) -> ephp_context:set(Context, G, undefined)
                  end, Getters),
    undefined.

-spec array_unique(context(), line(), Array :: var_value(),
                   Flags :: var_value()) -> ephp_array().

array_unique(_Context, _Line, {_, Array}, {_, Flags}) ->
    %% TODO error when Flags is not a SORT_* valid value
    ephp_array:from_list(unique(ephp_array:to_list(Array), [], Flags)).

-spec array_change_key_case(context(), line(), Array :: var_value(),
                            Flags :: var_value()) -> ephp_array().

array_change_key_case(_Context, _Line, {_, Array}, {_, Flags}) ->
    ephp_array:map(fun(K, V) ->
        if
            Flags >= 1 -> {ephp_string:to_upper(K), V};
            true -> {ephp_string:to_lower(K), V}
        end
    end, Array).

-spec array_chunk(context(), line(), Array :: var_value(), Size :: var_value(),
                  PreserveKeys :: var_value())  -> ephp_array().

array_chunk(_Context, _Line, {_, Array}, {_, Size}, {_, PreserveKeys}) ->
    case ephp_array:size(Array) > Size of
        false ->
            Array;
        true ->
            ephp_array:from_list(chunk(ephp_array:to_list(Array),
                                       Size, PreserveKeys))
    end.

-spec array_column(context(), line(), var_value(), var_value(), var_value()) ->
      false | ephp_array().
%% @doc in an array of arrays it retries the subelements with the key passed
%%      as a param. It let you to change the new key to retrieve the elements.
%% @end
array_column(Context, Line, _Array, {_, ColKey}, _IdxKey) when
        not (is_binary(ColKey) orelse is_number(ColKey)) ->
    File = ephp_context:get_active_file(Context),
    Error = {error, eshouldbe, Line, File, ?E_WARNING,
             {<<"array_column">>, <<"The column key">>,
              <<"either a string or an integer">>}},
    ephp_error:handle_error(Context, Error),
    false;

array_column(Context, Line, _Array, _ColKey, {_, IdxKey}) when
        not (is_binary(IdxKey) orelse is_number(IdxKey) orelse
             IdxKey =:= undefined) ->
    File = ephp_context:get_active_file(Context),
    Error = {error, eshouldbe, Line, File, ?E_WARNING,
             {<<"array_column">>, <<"The index key">>,
              <<"either a string or an integer">>}},
    ephp_error:handle_error(Context, Error),
    false;

array_column(_Context, _Line, {_, Array}, {_, ColKey}, {_, IdxKey}) ->
    ephp_array:fold(fun(_, V, A) ->
        case ephp_array:find(ColKey, V) of
            {ok, Val} ->
                Key = case IdxKey of
                    undefined -> auto;
                    _ -> ephp_array:find(IdxKey, V, auto)
                end,
                ephp_array:store(Key, Val, A);
            error ->
                A
        end
    end, ephp_array:new(), Array).


-spec reset(context(), line(), Array::var_value()) -> false | ephp_array().
%% @doc resets the cursor for an array moving it to the first element and
%%      retrieving that element if it's exists, false otherwise.
%% @end
reset(Context, _Line, {Var, Array}) ->
    case ephp_array:first(Array) of
        {error, _} ->
            false;
        {ok, {_Key, Value}, NewArray} ->
            ephp_context:set(Context, Var, NewArray),
            Value
    end.


-spec current(context(), line(), Array::var_value()) -> false | ephp_array().
%% @doc retrieve the current element under the cursor for an array.
current(_Context, _Line, {_, Array}) ->
    case ephp_array:current(Array) of
        {error, _} -> false;
        {ok, {_Key, Value}} -> Value
    end.


-spec php_end(context(), line(), Array::var_value()) -> false | ephp_array().
%% @doc moves the array cursor to the last element and retrieves it.
php_end(Context, _Line, {Var, Array}) ->
    case ephp_array:last(Array) of
        {error, _} ->
            false;
        {ok, {_Key, Value}, NewArray} ->
            ephp_context:set(Context, Var, NewArray),
            Value
    end.


-spec prev(context(), line(), Array::var_value()) -> false | ephp_array().
%% @doc moves the cursor to the previous element and retrieves it if it's
%%      possible, false otherwise.
%% @end
prev(Context, _Line, {Var, Array}) ->
    case ephp_array:prev(Array) of
        {error, bof} ->
            ephp_context:set(Context, Var, ephp_array:cursor(Array, false)),
            false;
        {error, _} ->
            false;
        {ok, {_Key, Value}, NewArray} ->
            ephp_context:set(Context, Var, NewArray),
            Value
    end.


-spec next(context(), line(), Array::var_value()) -> false | mixed().
%% @doc moves the cursor to the next element and retrieves it if it's possible,
%%      false otherwise.
%% @end
next(Context, _Line, {Var, Array}) ->
    case ephp_array:next(Array) of
        {error, eof} ->
            ephp_context:set(Context, Var, ephp_array:cursor(Array, false)),
            false;
        {error, _} ->
            false;
        {ok, {_Key, Value}, NewArray} ->
            ephp_context:set(Context, Var, NewArray),
            Value
    end.


-spec key(context(), line(), Array::var_value()) -> undefined | mixed().
%% @doc returns the key under the cursor or undefined if an error happens.
key(_Context, _Line, {_, Array}) ->
    case ephp_array:current(Array) of
        {error, _} -> undefined;
        {ok, {Key, _Value}} -> Key
    end.


-spec ksort(context(), line(), Array::var_value(), SortType::var_value()) -> boolean().
%% @doc order the array based on the keys modifying the original. The function
%%      returns true if the ordering was ok, otherwise false. We can use different
%%      sort types: SORT_REGULAR (default), SORT_NUMERIC, SORT_STRING,
%%      SORT_LOCALE_STRING, SORT_NATURAL, SORT_FLAG_CASE.
ksort(Context, _Line, {ArrayVar, Array}, {_, SortType}) ->
    ephp_context:set(Context, ArrayVar, ephp_array:ksort(Array, SortType)).


-spec array_keys(context(), line(), Array::var_value()) -> ephp_array().
%% @doc returns a new array with the keys.
array_keys(_Context, _Line, {_, Array}) ->
    ephp_array:keys(Array).


-spec array_pop(context(), line(), Array::var_value()) -> ephp_array().
%% @doc returns the last element of the array and removes it from the array.
array_pop(Context, _Line, {VarArray, Array}) ->
    {Head, TailArray} = ephp_array:pop(Array),
    ephp_context:set(Context, VarArray, TailArray),
    Head.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

chunk(Array, N, true) when length(Array) =< N ->
    [ephp_array:from_list(Array)];
chunk(Array, N, false) when length(Array) =< N ->
    {_, NewArray} = lists:foldl(fun({_, V}, {K, Res}) ->
        {K+1, Res ++ [{K,V}]}
    end, {0, []}, Array),
    [ephp_array:from_list(NewArray)];
chunk(Array, N, PrKeys) ->
    {A1, A2} = lists:split(N, Array),
    chunk(A1, N, PrKeys) ++ chunk(A2, N, PrKeys).

unique([], Array, _Flags) ->
    Array;
unique([{Key, Val}|Rest], Array, ?SORT_REGULAR) ->
    Check = fun({_, V}) ->
        ephp_data:is_equal(V, Val)
    end,
    case lists:any(Check, Array) of
        true -> unique(Rest, Array, ?SORT_REGULAR);
        false -> unique(Rest, Array ++ [{Key, Val}], ?SORT_REGULAR)
    end;
unique([{Key, Val}|Rest], Array, ?SORT_NUMERIC) ->
    NumArray = [ {K, ephp_data:to_number(V)} || {K, V} <- Array ],
    NumVal = ephp_data:to_number(Val),
    Check = fun({_, V}) ->
        V == NumVal
    end,
    case lists:any(Check, NumArray) of
        true -> unique(Rest, Array, ?SORT_NUMERIC);
        false -> unique(Rest, Array ++ [{Key, Val}], ?SORT_NUMERIC)
    end;
unique([{Key, Val}|Rest], Array, _Flags) ->
    StrArray = [ {K, ephp_data:to_bin(V)} || {K, V} <- Array ],
    StrVal = ephp_data:to_bin(Val),
    case lists:keyfind(StrVal, 2, StrArray) of
        {_, StrVal} -> unique(Rest, Array, ?SORT_STRING);
        false -> unique(Rest, Array ++ [{Key, Val}], ?SORT_STRING)
    end.
%% TODO: SORT_LOCALE_STRING

member(Value, Dict, true) ->
    List = ephp_array:to_list(Dict),
    lists:keysearch(Value, 2, List) =/= false;

member(Value, Dict, false) ->
    List = ephp_array:to_list(Dict),
    lists:any(fun({_, Member}) ->
        ephp_data:is_equal(Member, Value)
    end, List).

-spec array_merge(context(), line(), pos_integer(), Arrays :: [var_value()]) ->
    ephp_array().

array_merge(Context, Line, 1, [{_,Array}|_]) when not ?IS_ARRAY(Array) ->
    Data = {<<"array_merge">>, 1},
    File = ephp_context:get_active_file(Context),
    Error = {error, eargtype, Line, File, ?E_WARNING, Data},
    ephp_error:handle_error(Context, Error),
    undefined;
array_merge(Context, Line, N, [{_,_},{_,V}|_]) when not ?IS_ARRAY(V) ->
    Data = {<<"array_merge">>, N+1},
    File = ephp_context:get_active_file(Context),
    Error = {error, eargtype, Line, File, ?E_WARNING, Data},
    ephp_error:handle_error(Context, Error),
    undefined;
array_merge(_Context, _Line, _N, [{_,Array}]) ->
    Array;
array_merge(Context, Line, N, [{V1,A1},{_,A2}|Rest]) when ?IS_ARRAY(A2) ->
    Array = lists:foldl(fun
        ({K,V}, A) when is_integer(K) ->
            ephp_array:store(auto, V, A);
        ({K,V}, A) ->
            ephp_array:store(K, V, A)
    end, A1, ephp_array:to_list(A2)),
    array_merge(Context, Line, N+1, [{V1,Array}|Rest]).

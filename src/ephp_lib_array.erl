-module(ephp_lib_array).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    in_array/5,
    count/3,
    array_merge/3,
    list/3,
    array_unique/4,
    array_change_key_case/4,
    array_chunk/5,
    array_column/5
]).

-include("ephp.hrl").

-define(SORT_REGULAR, 0).
-define(SORT_NUMERIC, 1).
-define(SORT_STRING, 2).
-define(SORT_LOCALE_STRING, 5).

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
    ]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [
    {<<"SORT_REGULAR">>, ?SORT_REGULAR},
    {<<"SORT_NUMERIC">>, ?SORT_NUMERIC},
    {<<"SORT_STRING">>, ?SORT_STRING},
    {<<"SORT_LOCALE_STRING">>, ?SORT_LOCALE_STRING},
    {<<"CASE_LOWER">>, ?CASE_LOWER},
    {<<"CASE_UPPER">>, ?CASE_UPPER}
].

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

list(Context, _Line, [{#ephp_array{}=Array, undefined}|Getters]) ->
    zip_list(Context, ephp_array:to_list(Array), Getters),
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

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

chunk(Array, _, _) when length(Array) =:= 0 ->
    [];
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
unique([{Key,Val}|Rest], Array, ?SORT_REGULAR) ->
    Check = fun({_, V}) ->
        ephp_data:is_equal(V, Val)
    end,
    case lists:any(Check, Array) of
        true -> unique(Rest, Array, ?SORT_REGULAR);
        false -> unique(Rest, Array ++ [{Key, Val}], ?SORT_REGULAR)
    end;
unique([{Key,Val}|Rest], Array, ?SORT_NUMERIC) ->
    NumArray = [ {K, ephp_data:to_number(V)} || {K, V} <- Array ],
    NumVal = ephp_data:to_number(Val),
    case lists:keyfind(NumVal, 2, NumArray) of
        {_, NumVal} -> unique(Rest, Array, ?SORT_NUMERIC);
        false -> unique(Rest, Array ++ [{Key, Val}], ?SORT_NUMERIC)
    end;
unique([{Key,Val}|Rest], Array, _Flags) ->
    StrArray = [ {K, ephp_data:to_bin(V)} || {K, V} <- Array ],
    StrVal = ephp_data:to_bin(Val),
    case lists:keyfind(StrVal, 2, StrArray) of
        {_, StrVal} -> unique(Rest, Array, ?SORT_STRING);
        false -> unique(Rest, Array ++ [{Key, Val}], ?SORT_STRING)
    end.
%% TODO: SORT_LOCALE_STRING

zip_list(_Context, [], _) ->
    ok;
zip_list(_Context, _, []) ->
    ok;
zip_list(Context, [_|Array], [{[], _}|Getters]) ->
    zip_list(Context, Array, Getters);
zip_list(Context, [{_,Value}|Array], [{Getter,_}|Getters]) ->
    ephp_context:set(Context, Getter, Value),
    zip_list(Context, Array, Getters).

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
    end, A1, array_to_list(A2)),
    array_merge(Context, Line, N+1, [{V1,Array}|Rest]).

array_to_list(A) when ?IS_ARRAY(A) -> ephp_array:to_list(A);
array_to_list(A) when is_list(A) -> A.

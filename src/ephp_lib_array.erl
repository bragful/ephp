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
    array_unique/4
]).

-include("ephp.hrl").

-define(SORT_REGULAR, 0).
-define(SORT_NUMERIC, 1).
-define(SORT_STRING, 2).
-define(SORT_LOCALE_STRING, 5).

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {in_array, [{args, {2, 3, undefined, [mixed, array, {boolean, false}]}}]},
    count,
    {count, [{alias, <<"sizeof">>}]},
    {array_merge, [pack_args]},
    {list, [pack_args, {args, no_resolve}]},
    {array_unique, [
        {args, {1, 2, undefined, [array, {integer, ?SORT_STRING}]}}
    ]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [
    {<<"SORT_REGULAR">>, ?SORT_REGULAR},
    {<<"SORT_NUMERIC">>, ?SORT_NUMERIC},
    {<<"SORT_STRING">>, ?SORT_STRING},
    {<<"SORT_LOCALE_STRING">>, ?SORT_LOCALE_STRING}
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

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

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

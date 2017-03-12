-module(ephp_lib_array).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    in_array/4,
    count/3,
    array_merge/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    in_array,
    count,
    {count, [{alias, <<"sizeof">>}]},
    {array_merge, [pack_args]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec in_array(
    context(), line(),
    Key :: var_value(), Array :: var_value()) -> boolean().

in_array(_Context, _Line, {_,Value}, {_,Array}) ->
    member(Value, Array).

-spec count(context(), line(), Array :: var_value()) -> integer().

count(_Context, _Line, {_,Array}) when ?IS_ARRAY(Array) ->
    ephp_array:size(Array);

count(_Context, _Line, _Var) ->
    1.

-spec array_merge(context(), line(), Arrays :: [var_value()]) -> ephp_array().

array_merge(Context, Line, Args) ->
    array_merge(Context, Line, 1, Args).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

member(Value, Dict) ->
    List = ephp_array:to_list(Dict),
    lists:keysearch(Value, 2, List) =/= false.

-spec array_merge(context(), line(), pos_integer(), Arrays :: [var_value()]) ->
    ephp_array().

array_merge(Context, Line, 1, [{_,Array}|_]) when not ?IS_ARRAY(Array) ->
    Data = {<<"array_merge">>, 1, ephp_context:get_active_file(Context)},
    ephp_error:handle_error(Context, {error, eargtype, Line, ?E_WARNING, Data}),
    undefined;
array_merge(Context, Line, N, [{_,_},{_,V}|_]) when not ?IS_ARRAY(V) ->
    Data = {<<"array_merge">>, N+1, ephp_context:get_active_file(Context)},
    ephp_error:handle_error(Context, {error, eargtype, Line, ?E_WARNING, Data}),
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

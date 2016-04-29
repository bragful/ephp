-module(ephp_lib_math).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,

    php_round/3,
    php_ceil/3,
    php_floor/3,
    php_sqrt/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {php_ceil, [{alias, <<"ceil">>}]},
    {php_floor, [{alias, <<"floor">>}]},
    {php_round, [{alias, <<"round">>}]},
    {php_sqrt, [{alias, <<"sqrt">>}]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec php_ceil(context(), line(), var_value()) -> integer().

php_ceil(_Context, _Line, {_, Value}) when is_number(Value) ->
    ceiling(Value);
php_ceil(_Context, _Line, {_, Value}) ->
    ceiling(ephp_util:bin_to_float(Value)).

-spec php_floor(context(), line(), var_value()) -> integer().

php_floor(_Context, _Line, {_, Value}) when is_number(Value) ->
    floor(Value);
php_floor(_Context, _Line, {_, Value}) ->
    floor(ephp_util:bin_to_float(Value)).

-spec php_round(context(), line(), var_value()) -> integer().

php_round(_Context, _Line, {_, Value}) when is_number(Value) ->
    round(Value);
php_round(_Context, _Line, {_, Value}) ->
    round(ephp_util:bin_to_float(Value)).

-spec php_sqrt(context(), line(), var_value()) -> float().

php_sqrt(_Context, _Line, {_, Value}) when is_number(Value) ->
    math:sqrt(float(Value));
php_sqrt(_Context, _Line, {_, Value}) ->
    math:sqrt(ephp_util:bin_to_float(Value)).

-spec floor(number()) -> integer().

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).

-spec ceiling(number()) -> integer().

ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

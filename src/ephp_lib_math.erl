-module(ephp_lib_math).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    round/2,
    ceil/2,
    floor/2
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    ceil,
    floor,
    round
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec ceil(context(), var_value()) -> integer().

ceil(_Context, {_, Value}) when is_number(Value) ->
    ceiling(Value);
ceil(_Context, {_, Value}) ->
    ceiling(ephp_util:bin_to_float(Value)).

-spec floor(context(), var_value()) -> integer().

floor(_Context, {_, Value}) when is_number(Value) ->
    floor(Value);
floor(_Context, {_, Value}) when is_number(Value) ->
    floor(ephp_util:bin_to_float(Value)).

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
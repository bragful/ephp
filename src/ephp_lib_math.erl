-module(ephp_lib_math).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,

    php_round/3,
    php_ceil/3,
    php_floor/3,
    php_sqrt/3,
    php_abs/3,
    php_asin/3,
    php_acos/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {php_ceil, [{alias, <<"ceil">>}]},
    {php_floor, [{alias, <<"floor">>}]},
    {php_round, [{alias, <<"round">>}]},
    {php_sqrt, [{alias, <<"sqrt">>}]},
    {php_abs, [{alias, <<"abs">>}]},
    {php_acos, [{alias, <<"acos">>}]},
    {php_asin, [{alias, <<"asin">>}]},
    {php_atan, [{alias, <<"atan">>}]},
    {php_exp, [{alias, <<"exp">>}]},
    {php_max, [{alias, <<"max">>}]},
    {php_min, [{alias, <<"min">>}]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [
    {<<"M_PI">>, 3.14159265358979323846},
    {<<"M_E">>, 2.7182818284590452354},
    {<<"M_LOG2E">>, 1.4426950408889634074},
    {<<"M_LOG10E">>, 0.43429448190325182765},
    {<<"M_LN2">>, 0.69314718055994530942},
    {<<"M_LN10">>, 2.30258509299404568402},
    {<<"M_PI_2">>, 1.57079632679489661923},
    {<<"M_PI_4">>, 0.78539816339744830962},
    {<<"M_1_PI">>, 0.31830988618379067154},
    {<<"M_2_PI">>, 0.63661977236758134308},
    {<<"M_QSRTPI">>, 1.77245385090551602729},
    {<<"M_2_SQRTPI">>, 1.12837916709551257390},
    {<<"M_SQRT2">>, 1.41421356237309504880},
    {<<"M_SQRT3">>, 1.73205080756887729352},
    {<<"M_SQRT1_2">>, 0.70710678118654752440},
    {<<"M_LNPI">>, 1.14472988584940017414},
    {<<"M_EULER">>, 0.57721566490153286061},
    {<<"PHP_ROUND_HALF_UP">>, 1},
    {<<"PHP_ROUND_HALF_DOWN">>, 2},
    {<<"PHP_ROUND_HALF_EVEN">>, 3},
    {<<"PHP_ROUND_HALF_ODD">>, 4}
    %% TODO:
    % {<<"NAN">>, nan},
    % {<<"INFINITY">>, infinity}
].

-spec php_ceil(context(), line(), var_value()) -> integer().

php_ceil(_Context, _Line, {_, Value}) when is_number(Value) ->
    ceiling(Value);
php_ceil(_Context, _Line, {_, Value}) ->
    ceiling(ephp_data:bin_to_number(Value)).

-spec php_floor(context(), line(), var_value()) -> integer().

php_floor(_Context, _Line, {_, Value}) when is_number(Value) ->
    floor(Value);
php_floor(_Context, _Line, {_, Value}) ->
    floor(ephp_data:bin_to_number(Value)).

-spec php_round(context(), line(), var_value()) -> integer().

php_round(_Context, _Line, {_, Value}) when is_number(Value) ->
    round(Value);
php_round(_Context, _Line, {_, Value}) ->
    round(ephp_data:bin_to_number(Value)).

-spec php_sqrt(context(), line(), var_value()) -> float().

php_sqrt(_Context, _Line, {_, Value}) when is_number(Value) ->
    math:sqrt(float(Value));
php_sqrt(Context, Line, {_, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"sqrt">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

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

-spec php_abs(context(), line(), number()) -> number().

php_abs(_Context, _Line, {_, Number}) when is_number(Number)  ->
    abs(Number);

php_abs(_Context, _Line, {_, String}) when is_binary(String) ->
    abs(ephp_data:bin_to_number(String));

php_abs(_Context, _Line, _NotANumber) ->
    false.

-spec php_acos(context(), line(), number()) -> float().

php_acos(_Context, _Line, {_, Number}) when is_number(Number) ->
    math:acos(Number);

php_acos(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"acos">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec php_asin(context(), line(), number()) -> float().

php_asin(_Context, _Line, {_, Number}) when is_number(Number) ->
    math:asin(Number);

php_asin(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"asin">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

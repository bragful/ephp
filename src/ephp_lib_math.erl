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
    php_asinh/3,
    php_atan/3,
    php_atan2/4,
    php_atanh/3,
    php_acos/3,
    php_acosh/3,
    bindec/3,
    php_sin/3,
    php_sinh/3,
    php_tan/3,
    php_tanh/3,
    php_cos/3,
    php_cosh/3
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
    {php_acosh, [{alias, <<"acosh">>}]},
    {php_asin, [{alias, <<"asin">>}]},
    {php_asinh, [{alias, <<"asinh">>}]},
    {php_atan, [{alias, <<"atan">>}]},
    {php_atan2, [{alias, <<"atan2">>}]},
    {php_atanh, [{alias, <<"atanh">>}]},
    {php_exp, [{alias, <<"exp">>}]},
    {php_max, [{alias, <<"max">>}]},
    {php_min, [{alias, <<"min">>}]},
    bindec,
    {php_cos, [{alias, <<"cos">>}]},
    {php_cosh, [{alias, <<"cosh">>}]},
    {php_sin, [{alias, <<"sin">>}]},
    {php_sinh, [{alias, <<"sinh">>}]},
    {php_tan, [{alias, <<"tan">>}]},
    {php_tanh, [{alias, <<"tanh">>}]}
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
    ephp_data:ceiling(Value);
php_ceil(_Context, _Line, {_, Value}) ->
    ephp_data:ceiling(ephp_data:bin_to_number(Value)).

-spec php_floor(context(), line(), var_value()) -> integer().

php_floor(_Context, _Line, {_, Value}) when is_number(Value) ->
    ephp_data:floor(Value);
php_floor(_Context, _Line, {_, Value}) ->
    ephp_data:floor(ephp_data:bin_to_number(Value)).

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

-spec php_acosh(context(), line(), number()) -> float().

php_acosh(_Context, _Line, {_Var, Number}) when is_number(Number) ->
    math:acosh(Number);

php_acosh(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"acosh">>, 1, <<"double">>, ephp_data:gettype(Val), File},
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

-spec php_asinh(context(), line(), number()) -> float().

php_asinh(_Context, _Line, {_, Number}) when is_number(Number) ->
    math:asinh(Number);

php_asinh(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"asinh">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec php_atan(context(), line(), number()) -> float().

php_atan(_Context, _Line, {_, Number}) when is_number(Number) ->
    math:atan(Number);

php_atan(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"atan">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec php_atanh(context(), line(), number()) -> float().

php_atanh(_Context, _Line, {_, Number}) when is_number(Number) ->
    math:atanh(Number);

php_atanh(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"atanh">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec php_atan2(context(), line(), number(), number()) -> float().

php_atan2(_Context, _Line, {_, Number1}, {_, Number2}) when is_number(Number1)
                                                    andalso is_number(Number2) ->
    math:atan2(Number1, Number2);

php_atan2(Context, Line, {_Var, Val}, _) when not is_number(Val) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"atan2">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined;

php_atan2(Context, Line, _, {_Var, Val}) when not is_number(Val) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"atan2">>, 2, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

filter_bin(<<>>, B) ->
    B;
filter_bin(<<A:8,Rest/binary>>, B) when A =:= $0 orelse A =:= $1 ->
    filter_bin(Rest, <<B/binary,A:8>>);
filter_bin(<<_/utf8,Rest/binary>>, B) ->
    filter_bin(Rest, B).

-spec bindec(context(), line(), var_value()) -> float().

bindec(_Context, _Line, {_, String}) when is_binary(String) ->
    Filtered = filter_bin(String, <<"0">>),
    binary_to_integer(Filtered, 2);

bindec(Context, Line, {Var, A}) when ?IS_ARRAY(A) ->
    Level = ?E_NOTICE,
    File = ephp_context:get_active_file(Context),
    Type = <<"string">>,
    Data = {File, Type},
    ephp_error:handle_error(Context, {error, earrayconv, Line, Level, Data}),
    bindec(Context, Line, {Var, <<>>});

bindec(Context, Line, {_, #reg_instance{class=#class{name=ClassName}}}) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:error({error, enotostring, Line,
                      ?E_RECOVERABLE_ERROR, {File, ClassName}}),
    undefined;

bindec(Context, Line, {Var, Other}) ->
    bindec(Context, Line, {Var, ephp_data:to_bin(Other)}).

-spec php_cos(context(), line(), number()) -> float().

php_cos(_Context, _Line, {_, Number}) when is_number(Number) ->
    math:cos(Number);

php_cos(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"cos">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec php_cosh(context(), line(), number()) -> float().

php_cosh(_Context, _Line, {_Var, Number}) when is_number(Number) ->
    math:cosh(Number);

php_cosh(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"cosh">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec php_sin(context(), line(), number()) -> float().

php_sin(_Context, _Line, {_, Number}) when is_number(Number) ->
    math:sin(Number);

php_sin(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"sin">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec php_sinh(context(), line(), number()) -> float().

php_sinh(_Context, _Line, {_, Number}) when is_number(Number) ->
    math:sinh(Number);

php_sinh(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"sinh">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec php_tan(context(), line(), number()) -> float().

php_tan(_Context, _Line, {_, Number}) when is_number(Number) ->
    math:tan(Number);

php_tan(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"tan">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec php_tanh(context(), line(), number()) -> float().

php_tanh(_Context, _Line, {_, Number}) when is_number(Number) ->
    math:tanh(Number);

php_tanh(Context, Line, {_Var, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"tanh">>, 1, <<"double">>, ephp_data:gettype(Val), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

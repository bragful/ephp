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
    php_cosh/3,
    php_pow/4,
    base_convert/5,
    pi/2
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {php_ceil, [{alias, <<"ceil">>}]},
    {php_floor, [{alias, <<"floor">>}]},
    {php_round, [{alias, <<"round">>}]},
    {php_sqrt, [{args, [double]}, {alias, <<"sqrt">>}]},
    {php_abs, [{alias, <<"abs">>}]},
    {php_acos, [{args, [double]}, {alias, <<"acos">>}]},
    {php_acosh, [{args, [double]}, {alias, <<"acosh">>}]},
    {php_asin, [{args, [double]}, {alias, <<"asin">>}]},
    {php_asinh, [{args, [double]}, {alias, <<"asinh">>}]},
    {php_atan, [{args, [double]}, {alias, <<"atan">>}]},
    {php_atan2, [{args, [double, double]}, {alias, <<"atan2">>}]},
    {php_atanh, [{args, [double]}, {alias, <<"atanh">>}]},
    {php_exp, [{alias, <<"exp">>}]},
    {php_max, [{alias, <<"max">>}]},
    {php_min, [{alias, <<"min">>}]},
    bindec,
    {php_cos, [{args, [double]}, {alias, <<"cos">>}]},
    {php_cosh, [{args, [double]}, {alias, <<"cosh">>}]},
    {php_sin, [{args, [double]}, {alias, <<"sin">>}]},
    {php_sinh, [{args, [double]}, {alias, <<"sinh">>}]},
    {php_tan, [{args, [double]}, {alias, <<"tan">>}]},
    {php_tanh, [{args, [double]}, {alias, <<"tanh">>}]},
    {php_pow, [{alias, <<"pow">>}]},
    base_convert,
    pi
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [
    {<<"precision">>, 14}
].

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
    {<<"PHP_ROUND_HALF_ODD">>, 4},
    {<<"NAN">>, nan},
    {<<"INF">>, infinity},

    {<<"PHP_INT_SIZE">>, ?PHP_INT_SIZE},
    {<<"PHP_INT_MAX">>, ?PHP_INT_MAX}
].

-spec php_ceil(context(), line(), var_value()) -> integer().

php_ceil(_Context, _Line, {_, Value}) when is_number(Value) ->
    ephp_data:ceiling(Value);
php_ceil(_Context, _Line, {_, Value}) ->
    ephp_data:ceiling(ephp_data:bin_to_number(Value)).

-spec php_floor(context(), line(), var_value()) -> integer().

php_floor(_Context, _Line, {_, Value}) when is_number(Value) ->
    ephp_data:flooring(Value);
php_floor(_Context, _Line, {_, Value}) ->
    ephp_data:flooring(ephp_data:bin_to_number(Value)).

-spec php_round(context(), line(), var_value()) -> integer().

php_round(_Context, _Line, {_, Value}) when is_number(Value) ->
    round(Value);
php_round(_Context, _Line, {_, Value}) ->
    round(ephp_data:bin_to_number(Value)).

-spec php_sqrt(context(), line(), var_value()) -> float().

php_sqrt(_Context, _Line, {_, Value}) ->
    math:sqrt(float(Value)).

-spec php_abs(context(), line(), number()) -> number().

php_abs(_Context, _Line, {_, Number}) when is_number(Number)  ->
    abs(Number);

php_abs(_Context, _Line, {_, String}) when is_binary(String) ->
    abs(ephp_data:bin_to_number(String));

php_abs(_Context, _Line, _NotANumber) ->
    false.

-spec php_acos(context(), line(), number()) -> float().

php_acos(_Context, _Line, {_, Number}) ->
    case catch math:acos(Number) of
        {'EXIT', {badarith, _}} -> nan;
        Res when is_float(Res) -> Res
    end.

-spec php_acosh(context(), line(), number()) -> float().

php_acosh(_Context, _Line, {_Var, Number}) ->
    math:acosh(Number).

-spec php_asin(context(), line(), number()) -> float().

php_asin(_Context, _Line, {_, Number}) ->
    math:asin(Number).

-spec php_asinh(context(), line(), number()) -> float().

php_asinh(_Context, _Line, {_, Number}) ->
    math:asinh(Number).

-spec php_atan(context(), line(), number()) -> float().

php_atan(_Context, _Line, {_, Number}) ->
    math:atan(Number).

-spec php_atanh(context(), line(), number()) -> float().

php_atanh(_Context, _Line, {_, Number}) ->
    % math:atanh(Number); % FIXME: the implementation is not reliable
    1 / 2 * math:log((1 + Number) / (1 - Number)).

-spec php_atan2(context(), line(), number(), number()) -> float().

php_atan2(_Context, _Line, {_, Number1}, {_, Number2}) ->
    math:atan2(Number1, Number2).

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
    Error = {error, earrayconv, Line, File, Level, {Type}},
    ephp_error:handle_error(Context, Error),
    bindec(Context, Line, {Var, <<>>});

bindec(_Context, Line, {_, #ephp_object{class=#class{name=ClassName}}}) ->
    ephp_error:error({error, enotostring, Line,
                      ?E_RECOVERABLE_ERROR, {ClassName}});

bindec(Context, Line, {Var, Other}) ->
    bindec(Context, Line, {Var, ephp_data:to_bin(Other)}).

-spec php_cos(context(), line(), number()) -> float().

php_cos(_Context, _Line, {_, Number}) ->
    math:cos(Number).

-spec php_cosh(context(), line(), number()) -> float().

php_cosh(_Context, _Line, {_Var, Number}) ->
    math:cosh(Number).

-spec php_sin(context(), line(), number()) -> float().

php_sin(_Context, _Line, {_, Number}) ->
    math:sin(Number).

-spec php_sinh(context(), line(), number()) -> float().

php_sinh(_Context, _Line, {_, Number}) ->
    math:sinh(Number).

-spec php_tan(context(), line(), number()) -> float().

php_tan(_Context, _Line, {_, Number}) ->
    math:tan(Number).

-spec php_tanh(context(), line(), number()) -> float().

php_tanh(_Context, _Line, {_, Number}) ->
    math:tanh(Number).

-spec get_pow_value(context(), line(), ephp_object()) -> integer().

get_pow_value(Context, Line, #ephp_object{class=#class{name=ClassName}}) ->
    File = ephp_context:get_active_file(Context),
    Level = ?E_NOTICE,
    Data = {ClassName, <<"int">>},
    Error =  {error, enocast, Line, File, Level, Data},
    ephp_error:handle_error(Context, Error),
    1;
get_pow_value(_Context, _Line, N) ->
    ephp_data:bin_to_number(ephp_data:to_bin(N)).

-spec php_pow(context(), line(), var_value(), var_value()) ->
    float() | integer().

php_pow(Context, Line, {_, Base}, {_, Power}) ->
    B = get_pow_value(Context, Line, Base),
    P = get_pow_value(Context, Line, Power),
    math:pow(B, P).

base_convert_error(Context, Line, ArgNum, ArgData) ->
    Level = ?E_WARNING,
    Function = <<"base_convert">>,
    WrongType = ephp_data:gettype(ArgData),
    File = ephp_context:get_active_file(Context),
    Data = {Function, ArgNum, <<"long">>, WrongType},
    Error = {error, ewrongarg, Line, File, Level, Data},
    ephp_error:handle_error(Context, Error),
    <<>>.

base_convert_invalid(Context, Line, Spec, Val) ->
    Level = ?E_WARNING,
    Function = <<"base_convert">>,
    File = ephp_context:get_active_file(Context),
    Data = {Function, Spec, Val},
    Error = {error, einvalid, Line, File, Level, Data},
    ephp_error:handle_error(Context, Error),
    <<>>.

base(N) ->
    lists:sublist([
        $0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
        $a, $b, $c, $d, $e, $f, $g, $h, $i, $j,
        $k, $l, $m, $n, $o, $p, $q, $r, $s, $t,
        $u, $v, $w, $x, $y, $z
    ], N).

filter_base(Text, Base) ->
    filter_base(Text, base(Base), <<"0">>).

filter_base(<<>>, _Base, Filtered) ->
    Filtered;
filter_base(<<A:8,Rest/binary>>, Base, Filtered) when A >= $A andalso A =< $Z ->
    Char = A + ($a - $A),
    filter_base(<<Char:8,Rest/binary>>, Base, Filtered);
filter_base(<<A:8,Rest/binary>>, Base, Filtered) ->
    case lists:member(A, Base) of
        true -> filter_base(Rest, Base, <<Filtered/binary,A:8>>);
        false -> filter_base(Rest, Base, Filtered)
    end.

-spec base_convert(context(), line(), var_value(), var_value(), var_value()) ->
    binary().

base_convert(Context, Line, _, {_,From}, _) when not is_integer(From) ->
    base_convert_error(Context, Line, 2, From);

base_convert(Context, Line, _, _, {_,To}) when not is_integer(To) ->
    base_convert_error(Context, Line, 3, To);

base_convert(Context, Line, _, {_,From}, _) when
        From < 2 orelse From > 36 ->
    base_convert_invalid(Context, Line, <<"from base">>, From);

base_convert(Context, Line, _, _, {_,To}) when
        To < 2 orelse To > 36 ->
    base_convert_invalid(Context, Line, <<"to base">>, To);

base_convert(_Context, _Line, {_, String}, {_,From}, {_,To}) when
        is_binary(String) ->
    Filtered = filter_base(String, From),
    To10 = binary_to_integer(Filtered, From),
    integer_to_binary(To10, To);

base_convert(Context, Line, {Var, A}, From, To) when ?IS_ARRAY(A) ->
    Level = ?E_NOTICE,
    File = ephp_context:get_active_file(Context),
    Type = <<"string">>,
    Error = {error, earrayconv, Line, File, Level, {Type}},
    ephp_error:handle_error(Context, Error),
    base_convert(Context, Line, {Var, <<>>}, From, To);

base_convert(_Context, Line, {_, #ephp_object{class=#class{name=ClassName}}},
             _From, _To) ->
    ephp_error:error({error, enotostring, Line,
                      ?E_RECOVERABLE_ERROR, {ClassName}});

base_convert(Context, Line, {Var, Other}, From, To) ->
    base_convert(Context, Line, {Var, ephp_data:to_bin(Other)}, From, To).

-spec pi(context(), line()) -> float().

pi(Context, Line) ->
    ephp_context:get_const(Context, <<"M_PI">>, Line).

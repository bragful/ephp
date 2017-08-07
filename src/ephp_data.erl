-module(ephp_data).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    gettype/1,
    is_equal/2,
    to_bin/1,
    to_bin/3,
    to_int/1,
    to_int/3,
    to_float/1,
    to_float/3,
    to_boolean/1,
    bin_to_number/1,
    bin_to_number/2,
    increment_code/1,
    to_bool/1,
    zero_if_undef/1,
    pad_to_bin/2,
    ceiling/1,
    flooring/1
]).

-spec gettype(mixed()) -> binary().

gettype(Value) when is_boolean(Value) -> <<"boolean">>;
gettype(Value) when is_integer(Value) -> <<"integer">>;
gettype(Value) when is_float(Value) -> <<"float">>;
gettype(Value) when is_binary(Value) -> <<"string">>;
gettype(Value) when ?IS_ARRAY(Value) -> <<"array">>;
gettype(Value) when ?IS_OBJECT(Value) -> <<"object">>;
gettype(Value) when is_pid(Value) -> <<"resource">>;
gettype(Value) when ?IS_FUNCTION(Value) -> <<"object">>;
gettype(undefined) -> <<"NULL">>;
gettype(infinity) -> <<"float">>;
gettype(nan) -> <<"float">>.

-spec is_equal(A :: mixed(), B :: mixed()) -> boolean().

is_equal(A, B) ->
    At = gettype(A),
    Bt = gettype(B),
    case At =:= Bt of
        true ->
            A == B;
        false ->
            case {At,Bt} of
                {<<"integer">>,<<"string">>} -> A == bin_to_number(B);
                {<<"string">>,<<"integer">>} -> bin_to_number(A) == B;
                {<<"float">>,<<"string">>} -> A == bin_to_number(B);
                {<<"string">>,<<"float">>} -> bin_to_number(A) == B;
                {<<"boolean">>,_} -> A == to_bool(B);
                {_,<<"boolean">>} -> to_bool(A) == B;
                _ -> A == B
            end
    end.

-spec bin_to_number(binary(), Force :: true) -> integer() | float();
                   (binary(), Force :: false) -> integer() | float() | undefined.

bin_to_number(Bin, Force) ->
    {ok, R} = re:compile("^[+-]?[0-9]*(\\.[0-9]+(e[+-]?[1-9][0-9]*)?)?"),
    case re:run(Bin, R, [{capture, all, binary}]) of
        {match, [<<>>]} when Force -> 0;
        {match, [<<>>]} -> undefined;
        {match, [<<A:8>>]} when Force andalso (A =:= $+ orelse A =:= $-) -> 0;
        {match, [<<A:8>>]} when A =:= $+ orelse A =:= $- -> undefined;
        {match, [Num]} -> binary_to_integer(Num);
        {match, [Num|_]} -> binary_to_float(Num)
    end.

-spec bin_to_number(binary()) -> integer() | float().

bin_to_number(Bin) when is_binary(Bin) ->
    bin_to_number(Bin, true).

-spec to_int(A :: mixed()) -> integer().

to_int(A) when is_binary(A) -> bin_to_number(A);

to_int(A) when is_integer(A) -> A;

to_int(A) when is_float(A) -> flooring(A);

to_int(A) when ?IS_ARRAY(A) ->
    case ephp_array:size(A) of
        0 -> 0;
        _ -> 1
    end;

to_int(true) -> 1;

% FIXME: conversion from NAN or INF to int is a bit tricky...
to_int(infinity) -> ?PHP_INT_MIN;

to_int(nan) -> ?PHP_INT_MIN;

to_int(_) -> 0.

-spec to_int(context(), line(), mixed()) -> integer().

to_int(Ctx, Line, #obj_ref{pid = Objects, ref = ObjectId}) ->
    ClassName = ephp_object:get_class_name(Objects, ObjectId),
    File = ephp_context:get_active_file(Ctx),
    Data = {ClassName, <<"int">>},
    ephp_error:handle_error(Ctx, {error, enocast, Line, File, ?E_NOTICE, Data}),
    1;

to_int(_Ctx, _Line, Val) ->
    to_int(Val).

-spec to_float(mixed()) -> float().

to_float(I) when is_integer(I) ->
    erlang:float(I);

to_float(T) when is_binary(T) ->
    erlang:float(ephp_data:bin_to_number(T));

to_float(F) when is_float(F) ->
    F;

to_float(A) when ?IS_ARRAY(A) ->
    case ephp_array:size(A) of
        0 -> 0.0;
        _ -> 1.0
    end;

to_float(true) -> 1.0;

to_float(false) -> 0.0;

to_float(infinity) -> infinity;

to_float(nan) -> nan;

to_float(undefined) -> 0.0.

-spec to_float(context(), line(), mixed()) -> float().

to_float(Ctx, Line, #obj_ref{pid = Objects, ref = ObjectId}) ->
    ClassName = ephp_object:get_class_name(Objects, ObjectId),
    File = ephp_context:get_active_file(Ctx),
    Data = {ClassName, <<"double">>},
    ephp_error:handle_error(Ctx, {error, enocast, Line, File, ?E_NOTICE, Data}),
    1.0;

to_float(_Context, _Line, Value) ->
    to_float(Value).

-spec to_bin(A :: mixed()) -> binary().

to_bin(A) when is_binary(A) ->
    A;

to_bin(A) when ?IS_ARRAY(A) ->
    <<"Array">>;

to_bin(A) when is_list(A) ->
    list_to_binary(A);

to_bin(A) when is_integer(A) ->
    integer_to_binary(A);

to_bin(A) when is_float(A) ->
    Precision = ephp_config:get(<<"precision">>),
    case flooring(A) of
        F when A-F == 0 -> integer_to_binary(F);
        _ -> float_to_binary(A, [{decimals, Precision}, compact])
    end;

to_bin(true) -> <<"1">>;

to_bin(false) -> <<>>;

to_bin(nan) -> <<"NAN">>;

to_bin(infinity) -> <<"INF">>;

to_bin(undefined) -> <<>>;

to_bin(MemRef) when ?IS_MEM(MemRef) -> to_bin(ephp_mem:get(MemRef)).

-spec to_bin(context(), line(), mixed()) -> binary().

to_bin(Ctx, Line, Array) when ?IS_ARRAY(Array) ->
    File = ephp_context:get_active_file(Ctx),
    Data = {<<"string">>},
    Error = {error, earrayconv, Line, File, ?E_NOTICE, Data},
    ephp_error:handle_error(Ctx, Error),
    <<"Array">>;

to_bin(Context, Line, #obj_ref{} = ObjRef) ->
    #ephp_object{class = #class{name = ClassName}} = ephp_object:get(ObjRef),
    try
        Call = #call{name = <<"__toString">>, line = Line},
        ephp_context:call_method(Context, ObjRef, Call)
    catch
        throw:{error, eundefmethod, _, _, {ClassName, <<"__toString">>}} ->
            Data = {ClassName},
            Error = {error, enotostring, Line, ?E_RECOVERABLE_ERROR, Data},
            ephp_error:error(Error)
    end;

to_bin(Context, Line, MemRef) when ?IS_MEM(MemRef) ->
    to_bin(Context, Line, ephp_mem:get(MemRef));

to_bin(_Context, _Line, Val) ->
    to_bin(Val).

-spec to_boolean(mixed()) -> binary().

to_boolean(0) -> false;

to_boolean(0.0) -> false;

to_boolean(N) when is_number(N) -> true;

to_boolean(<<>>) -> false;

to_boolean(S) when is_binary(S) -> true;

to_boolean(true) -> true;

to_boolean(false) -> false;

to_boolean(infinity) -> true;

to_boolean(nan) -> true;

to_boolean(undefined) -> false;

to_boolean(Array) when ?IS_ARRAY(Array) ->
    case ephp_array:size(Array) of
        0 -> false;
        _ -> true
    end;

to_boolean(#obj_ref{}) -> true.


-spec increment_code(Code :: binary()) -> integer() | binary().

increment_code(<<>>) ->
    1;

increment_code(Code) when is_binary(Code) ->
    S = byte_size(Code) - 1,
    <<H:S/binary,T:8/integer>> = Code,
    if
        (T >= $a andalso T < $z) orelse
        (T >= $A andalso T < $Z) orelse
        (T >= $0 andalso T < $9) ->
            <<H/binary,(T+1):8/integer>>;
        T =:= $z andalso H =/= <<>> ->
            NewH = increment_code(H),
            <<NewH/binary, "a">>;
        T =:= $z ->
            <<"aa">>;
        T =:= $Z andalso H =/= <<>> ->
            NewH = increment_code(H),
            <<NewH/binary, "A">>;
        T =:= $Z ->
            <<"AA">>;
        T =:= $9 andalso H =/= <<>> ->
            NewH = increment_code(H),
            <<NewH/binary, "0">>;
        T =:= $9 ->
            <<"10">>;
        true ->
            <<H/binary, T:8/integer>>
    end.

-spec to_bool(Value :: undefined | boolean() |
    ephp_array() | integer() | float() | string() | binary()) -> boolean().

to_bool(false) -> false;

to_bool(undefined) -> false;

to_bool(0) -> false;

to_bool(<<>>) -> false;

to_bool([]) -> false;

to_bool(Array) when ?IS_ARRAY(Array) -> ephp_array:new() =/= Array;

to_bool(_Other) -> true.


-spec zero_if_undef(Value :: undefined |
    ephp_array() | integer() | float() | string() | binary()) -> integer().

zero_if_undef(undefined) -> 0;

zero_if_undef(Value) when ?IS_ARRAY(Value) -> throw(einvalidop);

zero_if_undef(infinity) -> infinity;

zero_if_undef(nan) -> nan;

zero_if_undef(Value) when not is_number(Value) -> 0;

zero_if_undef(Value) -> Value.


-spec pad_to_bin(Num :: integer() | binary(), Pad :: integer()) -> binary().

pad_to_bin(Num, Pad) when Pad =< 0 andalso is_binary(Num) ->
    Num;

pad_to_bin(Num, Pad) when not is_binary(Num) ->
    NumBin = to_bin(Num),
    pad_to_bin(NumBin, Pad - byte_size(NumBin));

pad_to_bin(Num, Pad) ->
    pad_to_bin(<<"0",Num/binary>>, Pad-1).

-spec flooring(number()) -> integer().
-ifdef(NATIVE_FLOOR).
flooring(X) -> floor(X).
-else.
flooring(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
flooring(X) ->
    trunc(X).
-endif.

-spec ceiling(number()) -> integer().
-ifdef(NATIVE_CEIL).
ceiling(X) -> ceil(X).
-else.
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.
-endif.

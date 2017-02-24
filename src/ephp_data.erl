-module(ephp_data).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    gettype/1,
    is_equal/2,
    to_bin/1,
    to_bin/3,
    bin_to_number/1,
    increment_code/1,
    to_bool/1,
    zero_if_undef/1,
    pad_to_bin/2,
    ceiling/1,
    floor/1
]).

-spec gettype(mixed()) -> binary().

gettype(Value) when is_boolean(Value) -> <<"boolean">>;
gettype(Value) when is_integer(Value) -> <<"integer">>;
gettype(Value) when is_float(Value) -> <<"float">>;
gettype(Value) when is_binary(Value) -> <<"string">>;
gettype(Value) when ?IS_ARRAY(Value) -> <<"array">>;
gettype(Value) when is_record(Value, reg_instance) -> <<"object">>;
gettype(Value) when is_pid(Value) -> <<"resource">>;
gettype(Value) when is_record(Value, function) -> <<"object">>;
gettype(undefined) -> <<"NULL">>;
gettype(_) -> <<"unknown type">>.

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

-spec bin_to_number(binary()) -> integer() | float().

bin_to_number(Bin) when is_binary(Bin) ->
    {ok, R} = re:compile("^[+-]?[0-9]*(\.[0-9]+(e[+-]?[1-9][0-9]*)?)?"),
    case re:run(Bin, R, [{capture, all, binary}]) of
        {match, [<<>>]} -> 0;
        {match, [Num]} -> binary_to_integer(Num);
        {match, [Num|_]} -> binary_to_float(Num)
    end.

-spec to_bin(A :: binary() | string() | integer() | undefined) -> binary().

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
    case floor(A) of
        F when A-F == 0 -> integer_to_binary(F);
        _ -> float_to_binary(A, [{decimals, Precision}, compact])
    end;

to_bin(true) -> <<"1">>;

to_bin(false) -> <<>>;

to_bin(undefined) -> <<>>.

-spec to_bin(context(), line(),
             A :: binary() | string() | integer() | undefined) -> binary().

to_bin(Context, Line, #reg_instance{class=#class{name=CN}}=RegInstance) ->
    try
        Call = #call{name = <<"__toString">>},
        ephp_context:call_method(Context, RegInstance, Call)
    catch
        throw:{error,eundefmethod,_,_,<<"__toString">>} ->
            File = ephp_context:get_active_file(Context),
            ephp_error:error({error, enotostring, Line, ?E_ERROR, {File, CN}})
    end;

to_bin(_Context, _Line, Val) ->
    to_bin(Val).

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

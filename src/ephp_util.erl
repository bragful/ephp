-module(ephp_util).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    gettype/1,
    is_equal/2,
    to_bin/1,
    to_bin/3,
    to_lower/1,
    increment_code/1,
    to_bool/1,
    zero_if_undef/1,
    pad_to_bin/2,
    get_line/1,
    get_abbr_weekday/1,
    get_abbr_month/1,
    get_timestamp/1,
    get_month/1,
    get_weekday/1
]).

-include("ephp.hrl").

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
                {<<"integer">>,<<"string">>} -> A == bin_to_int(B);
                {<<"string">>,<<"integer">>} -> bin_to_int(A) == B;
                {<<"float">>,<<"string">>} -> A == bin_to_float(B);
                {<<"string">>,<<"float">>} -> bin_to_float(A) == B;
                {<<"boolean">>,_} -> A == to_bool(B);
                {_,<<"boolean">>} -> to_bool(A) == B;
                _ -> A == B
            end
    end.

-spec bin_to_int(binary()) -> integer().

bin_to_int(Bin) when is_binary(Bin) ->
    bin_to_int(Bin, <<>>).

bin_to_int(<<>>, <<>>) ->
    0;

bin_to_int(<<>>, I) ->
    binary_to_integer(I);

bin_to_int(<<"-",Rest/binary>>, <<>>) ->
    bin_to_int(Rest, <<"-">>);

bin_to_int(<<A:8/integer,Rest/binary>>, I) when A >= $0 andalso A =< $9 ->
    bin_to_int(Rest, <<I/binary, A:8/integer>>);

bin_to_int(_, <<>>) ->
    0;

bin_to_int(_, I) ->
    binary_to_integer(I).


-spec bin_to_float(binary()) -> float().

bin_to_float(Bin) when is_binary(Bin) ->
    bin_to_float(Bin, <<>>).

bin_to_float(<<>>, <<>>) ->
    0.0;

bin_to_float(<<>>, F) ->
    binary_to_float(F);

bin_to_float(<<"-",Rest/binary>>, <<>>) ->
    bin_to_float(Rest, <<"-">>);

bin_to_float(<<A:8/integer,Rest/binary>>, F) when
        (A >= $0 andalso A =< $9) orelse A == $. orelse A == $e ->
    bin_to_float(Rest, <<F/binary, A:8/integer>>);

bin_to_float(_, <<>>) ->
    0.0;

bin_to_float(_, F) ->
    binary_to_float(F).


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
    float_to_binary(A, [{decimals, 100}, compact]);

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

-spec to_lower(binary()) -> binary().

to_lower(Text) ->
    unistring:to_lower(Text).

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

-spec get_line(line() | undefined) -> non_neg_integer() | undefined.

get_line(undefined) ->
    undefined;

get_line({{line, Line}, {column, _Column}}) ->
    Line.

-spec get_timestamp(TS::integer() | float()) -> timer:timestamp().

get_timestamp(Timestamp) ->
    M = trunc(Timestamp) div 1000000,
    S = trunc(Timestamp) rem 1000000,
    U = trunc(Timestamp * 1000000) rem 1000000,
    {M,S,U}.

-spec get_abbr_month(M :: integer()) -> binary().

get_abbr_month(1) -> <<"Jan">>;
get_abbr_month(2) -> <<"Feb">>;
get_abbr_month(3) -> <<"Mar">>;
get_abbr_month(4) -> <<"Apr">>;
get_abbr_month(5) -> <<"May">>;
get_abbr_month(6) -> <<"Jun">>;
get_abbr_month(7) -> <<"Jul">>;
get_abbr_month(8) -> <<"Aug">>;
get_abbr_month(9) -> <<"Sep">>;
get_abbr_month(10) -> <<"Oct">>;
get_abbr_month(11) -> <<"Nov">>;
get_abbr_month(12) -> <<"Dec">>.

-spec get_month(M :: integer()) -> binary().

get_month(1) -> <<"January">>;
get_month(2) -> <<"February">>;
get_month(3) -> <<"March">>;
get_month(4) -> <<"April">>;
get_month(5) -> <<"May">>;
get_month(6) -> <<"June">>;
get_month(7) -> <<"July">>;
get_month(8) -> <<"August">>;
get_month(9) -> <<"September">>;
get_month(10) -> <<"October">>;
get_month(11) -> <<"November">>;
get_month(12) -> <<"December">>.

-spec get_abbr_weekday(D :: date()) -> binary().

get_abbr_weekday(0) -> <<"Sun">>;
get_abbr_weekday(1) -> <<"Mon">>;
get_abbr_weekday(2) -> <<"Tue">>;
get_abbr_weekday(3) -> <<"Wed">>;
get_abbr_weekday(4) -> <<"Thu">>;
get_abbr_weekday(5) -> <<"Fri">>;
get_abbr_weekday(6) -> <<"Sat">>;
get_abbr_weekday(7) -> <<"Sun">>.

-spec get_weekday(D :: date()) -> binary().

get_weekday(0) -> <<"Sunday">>;
get_weekday(1) -> <<"Monday">>;
get_weekday(2) -> <<"Tuesday">>;
get_weekday(3) -> <<"Wednesday">>;
get_weekday(4) -> <<"Thursday">>;
get_weekday(5) -> <<"Friday">>;
get_weekday(6) -> <<"Saturday">>;
get_weekday(7) -> <<"Sunday">>.

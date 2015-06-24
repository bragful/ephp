-module(ephp_util).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    to_bin/1,
    to_lower/1,
    increment_code/1,
    to_bool/1,
    zero_if_undef/1,
    pad_to_bin/2,
    get_line/1
]).

-include("ephp.hrl").

-spec to_bin(A :: binary() | string() | integer() | undefined) -> binary().

to_bin(A) when is_binary(A) ->
    A; 

to_bin(A) when ?IS_DICT(A) ->
    <<"Array">>;

to_bin(A) when is_list(A) -> 
    list_to_binary(A);

to_bin(A) when is_integer(A) -> 
    integer_to_binary(A);

to_bin(A) when is_float(A) -> 
    float_to_binary(A, [{decimals, 100}, compact]);

to_bin(true) -> <<"1">>;

to_bin(false) -> <<>>;

to_bin(null) -> <<>>;

to_bin(undefined) -> <<>>. 

-spec to_lower(binary()) -> binary().

to_lower(Text) ->
    list_to_binary(string:to_lower(binary_to_list(Text))).

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
    ?DICT_TYPE | integer() | float() | string() | binary()) -> boolean().

to_bool(false) -> false;

to_bool(undefined) -> false;

to_bool(0) -> false;

to_bool(<<>>) -> false;

to_bool([]) -> false;

to_bool(Array) when ?IS_DICT(Array) -> ?DICT:new() =/= Array;

to_bool(_Other) -> true.


-spec zero_if_undef(Value :: undefined | 
    ?DICT_TYPE | integer() | float() | string() | binary()) -> integer().

zero_if_undef(undefined) -> 0;

zero_if_undef(Value) when ?IS_DICT(Value) -> throw(einvalidop);

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

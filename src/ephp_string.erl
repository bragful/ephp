-module(ephp_string).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    to_lower/1,
    to_upper/1,
    escape/2,
    trim/1,
    trim/2,
    rtrim/2,
    ltrim/2,
    join/2,
    vsn_cmp/2,
    spaces/1,
    repeat/2,
    reverse/1,
    hex2bin/1,
    ihex2bin/1,
    bin2hex/1,
    ibin2hex/1
]).

-spec to_lower(binary() | undefined) -> binary() | undefined.

to_lower(undefined) ->
    undefined;

to_lower(Text) ->
    unistring:to_lower(Text).

-spec to_upper(binary()) -> binary();
              (undefined) -> undefined.

to_upper(undefined) ->
    undefined;

to_upper(Text) ->
    unistring:to_upper(Text).

-spec escape(mixed(), non_neg_integer()) -> binary().

escape(Bin, Escape) when is_binary(Bin) ->
    Bin2 = lists:foldl(fun
        (E, Acc) when E =:= Escape -> <<Acc/binary, "\\", Escape:8>>;
        (C, Acc) -> <<Acc/binary, C:8>>
    end, <<>>, binary_to_list(Bin)),
    <<"'", Bin2/binary, "'">>;
escape(MemRef, Escape) when ?IS_MEM(MemRef) ->
    escape(ephp_mem:get(MemRef), Escape);
escape(Mixed, _Escape) ->
    ephp_data:to_bin(Mixed).

-spec trim(binary()) -> binary();
          (undefined) -> undefined.

trim(undefined) ->
    undefined;

trim(Text) ->
    re:replace(Text, "^\\s+|\\s+$", "", [{return, binary}, global]).


-spec trim(binary(), [byte()]) -> binary().

trim(Text, Chars) ->
    rtrim(ltrim(Text, Chars), Chars).


-spec rtrim(binary(), [byte()]) -> binary().

rtrim(<<>>, _Chars) ->
    <<>>;
rtrim(<<A:8>>, Chars) ->
    case lists:member(A, Chars) of
        true -> <<>>;
        false -> <<A:8>>
    end;
rtrim(Text, Chars) ->
    Size = byte_size(Text) - 1,
    <<Rest:Size/binary,A:8>> = Text,
    case lists:member(A, Chars) of
        true -> rtrim(Rest, Chars);
        false -> Text
    end.

-spec ltrim(binary(), [byte()]) -> binary().

ltrim(<<>>, _Chars) ->
    <<>>;
ltrim(<<A:8>>, Chars) ->
    case lists:member(A, Chars) of
        true -> <<>>;
        false -> <<A:8>>
    end;
ltrim(<<A:8,Rest/binary>> = Text, Chars) ->
    case lists:member(A, Chars) of
        true -> ltrim(Rest, Chars);
        false -> Text
    end.

-spec join([binary()], binary()) -> binary().

join([], _Sep) ->
    <<>>;

join([Part], _Sep) ->
    Part;

join([Head|Tail], Sep) ->
    lists:foldl(fun (Value, Acc) ->
        <<Acc/binary, Sep/binary, Value/binary>>
    end, Head, Tail).

-spec vsn_cmp(binary(), binary()) -> integer();
             ([binary()], [binary()]) -> integer().

vsn_cmp(Vsn1, Vsn2) when is_binary(Vsn1) andalso is_binary(Vsn2) ->
    Vsn01 = hd(binary:split(Vsn1, <<"-">>)),
    Vsn1parts = binary:split(Vsn01, <<".">>, [global]),
    Vsn02 = hd(binary:split(Vsn2, <<"-">>)),
    Vsn2parts = binary:split(Vsn02, <<".">>, [global]),
    vsn_cmp([ binary_to_integer(B) || B <- Vsn1parts ],
            [ binary_to_integer(B) || B <- Vsn2parts ]);

vsn_cmp([], []) -> 0;
vsn_cmp([_|_], []) -> 1;
vsn_cmp([], [_|_]) -> -1;
vsn_cmp([A|_], [B|_]) when A > B -> 1;
vsn_cmp([A|_], [B|_]) when A < B -> -1;
vsn_cmp([A|ARest], [A|BRest]) -> vsn_cmp(ARest, BRest).

-spec spaces(pos_integer()) -> binary().
%% @doc generate as many spaces as the number passed as param.
spaces(Num) ->
    repeat(Num, 32).

-spec repeat(pos_integer(), byte()) -> binary().
%% @doc repeat the byte passed as param as many times as the number passed as param.
repeat(Num, Byte) ->
    repeat(Num, Byte, <<>>).

-spec repeat(pos_integer(), byte(), binary()) -> binary().
%% @doc repeat internal function.
%% @see repeat/2
%% @private
repeat(0, _Byte, Binary) -> Binary;
repeat(N, Byte, Binary) -> repeat(N-1, Byte, <<Binary/binary, Byte:8>>).

-spec reverse(binary()) -> binary().
%% @doc get the reverse of a string passed as param.
%%      Reference: [https://stackoverflow.com/a/43310493]
%% @end
reverse(Str) ->
    Size = erlang:bit_size(Str),
    <<StrInt:Size/integer-little>> = Str,
    <<StrInt:Size/integer-big>>.

-spec ihex2bin(binary()) -> binary().
%% @doc transform a hexadecimal string in (big-endian)
ihex2bin(<<A:8, B:8, Rest/binary>>) ->
    <<(binary_to_integer(<<B:8, A:8>>, 16)), (ihex2bin(Rest))/binary>>;
ihex2bin(<<A:8, B:8>>) ->
    <<(binary_to_integer(<<B:8, A:8>>, 16))>>;
ihex2bin(<<A:8>>) ->
    <<(binary_to_integer(<<$0:8, A:8>>, 16))>>;
ihex2bin(<<>>) ->
    <<>>.

-spec hex2bin(binary()) -> binary().
%% @doc transform a hexadecimal string in (little-endian)
hex2bin(<<A:16, Rest/binary>>) ->
    <<(binary_to_integer(<<A:16>>, 16)), (hex2bin(Rest))/binary>>;
hex2bin(<<A:16>>) ->
    <<(binary_to_integer(<<A:16>>, 16))>>;
hex2bin(<<A:8>>) ->
    <<(binary_to_integer(<<A:8, $0:8>>, 16))>>;
hex2bin(<<>>) ->
    <<>>.

-spec bin2hex(binary()) -> binary().
%% @doc transform a binary string in its hexadecimal representation.
bin2hex(Bin) when is_binary(Bin) ->
    Binaries = [ integer_to_list(X, 16) || <<X:4/integer>> <= Bin ],
    to_lower(iolist_to_binary(Binaries)).


-spec ibin2hex(binary()) -> binary().
%% @doc transform a binary string in its hexadecimal representation.
ibin2hex(Bin) when is_binary(Bin) ->
    ToInt = fun(X, Y) ->
        [ integer_to_list(Y, 16), integer_to_list(X, 16) ]
    end,
    Process = fun
        (_, <<X:4/integer, Y:4/integer>>) ->
            [ToInt(X, Y)];
        (P, <<X:4/integer, Y:4/integer, Rest/binary>>) ->
            [ToInt(X, Y)|P(P, Rest)]
    end,
    Binaries = Process(Process, Bin),
    to_lower(iolist_to_binary(Binaries)).

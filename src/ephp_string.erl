-module(ephp_string).

-author('manuel@altenwald.com').

-include("ephp.hrl").

-export([to_lower/1, to_upper/1, capitalize/2, escape/2, expand_mask/1, trim/1, trim/2,
         rtrim/2, ltrim/2, lpad/3, rpad/3, pad/3, join/2, vsn_cmp/2, spaces/1, repeat/2, reverse/1,
         hex2bin/1, ihex2bin/1, bin2hex/1, ibin2hex/1]).

-spec to_lower(integer()) -> integer();
              (binary()) -> binary();
              (undefined) -> undefined.
to_lower(undefined) ->
    undefined;
to_lower(Text) ->
    unistring:to_lower(Text).

-spec to_upper(integer()) -> integer();
              (binary()) -> binary();
              (undefined) -> undefined.
to_upper(undefined) ->
    undefined;
to_upper(Text) ->
    unistring:to_upper(Text).

-spec capitalize(binary(), [byte()]) -> binary().
capitalize(<<>>, _Sep) ->
    <<>>;
capitalize(<<A/utf8, Rest/binary>>, Sep) ->
    Text = <<(unistring:to_upper(<<A/utf8>>))/binary, Rest/binary>>,
    capitalize(Text, <<>>, Sep).

capitalize(<<>>, Result, _Sep) ->
    Result;
capitalize(<<A/utf8, Rest/binary>>, Result, Sep) ->
    case lists:member(A, Sep) of
        true when Rest =/= <<>> ->
            <<B/utf8, NewRest/binary>> = Rest,
            case lists:member(B, Sep) of
                true ->
                    capitalize(Rest, <<Result/binary, A/utf8>>, Sep);
                false ->
                    Cap = unistring:to_upper(<<B/utf8>>),
                    NewResult = <<Result/binary, A/utf8, Cap/binary>>,
                    capitalize(NewRest, NewResult, Sep)
            end;
        _ ->
            capitalize(Rest, <<Result/binary, A/utf8>>, Sep)
    end.

-spec escape(mixed(), non_neg_integer()) -> binary().
escape(Bin, Escape) when is_binary(Bin) ->
    Bin2 =
        lists:foldl(fun (E, Acc) when E =:= Escape ->
                            <<Acc/binary, "\\", Escape:8>>;
                        (C, Acc) ->
                            <<Acc/binary, C:8>>
                    end,
                    <<>>,
                    binary_to_list(Bin)),
    <<"'", Bin2/binary, "'">>;
escape(MemRef, Escape) when ?IS_MEM(MemRef) ->
    escape(ephp_mem:get(MemRef), Escape);
escape(Mixed, _Escape) ->
    ephp_data:to_bin(Mixed).

-ifdef(NATIVE_STR_REPLACE).

replace(M, Subject, Seq) ->
    string:replace(M, Subject, Seq).

-else.

replace(M, Subject, Seq) ->
    binary:replace(M, Subject, Seq).

-endif.

-spec expand_mask(binary()) -> binary().
expand_mask(Mask) ->
    Opts = [{capture, all, binary}, global],
    case re:run(Mask, "(.)\\.\\.(.)", Opts) of
        {match, Matches} ->
            lists:foldl(fun([Subject, <<Init:8>>, <<End:8>>], M) ->
                           Seq = list_to_binary(lists:seq(Init, End)),
                           iolist_to_binary(replace(M, Subject, Seq))
                        end,
                        Mask,
                        Matches);
        nomatch ->
            Mask
    end.

-spec pad(binary(), integer(), binary() | undefined) -> binary();
         (undefined, integer(), binary() | undefined) -> undefined.
pad(undefined, _Size, _PadStr) ->
    undefined;
pad(Bin, _Size, undefined) ->
    Bin;
pad(Bin, Size, _PadStr) when byte_size(Bin) >= Size ->
    Bin;
pad(Bin, Size, PadStr) ->
    MissingLen = Size - byte_size(Bin),
    SizeLeft = ephp_data:flooring(MissingLen / 2),
    NewBin = rpad(Bin, Size - SizeLeft, PadStr),
    lpad(NewBin, Size, PadStr).

-spec lpad(binary(), integer(), binary() | undefined) -> binary();
          (undefined, integer(), binary() | undefined) -> undefined.
lpad(undefined, _PadLen, _PadStr) ->
    undefined;
lpad(Input, _PadLen, undefined) ->
    Input;
lpad(Input, PadLen, _PadStr) when byte_size(Input) >= PadLen ->
    Input;
lpad(Input, PadLen, PadStr) ->
    MissingLen = PadLen - byte_size(Input),
    Times = ephp_data:ceiling(MissingLen / byte_size(PadStr)),
    <<Str:MissingLen/binary, _/binary>> = repeat(Times, PadStr),
    <<Str/binary, Input/binary>>.

-spec rpad(binary(), integer(), binary() | undefined) -> binary();
          (undefined, integer(), binary() | undefined) -> undefined.
rpad(undefined, _PadLen, _PadStr) ->
    undefined;
rpad(Input, _PadLen, undefined) ->
    Input;
rpad(Input, PadLen, _PadStr) when byte_size(Input) >= PadLen ->
    Input;
rpad(Input, PadLen, PadStr) ->
    MissingLen = PadLen - byte_size(Input),
    Times = ephp_data:ceiling(MissingLen / byte_size(PadStr)),
    <<Str:MissingLen/binary, _/binary>> = repeat(Times, PadStr),
    <<Input/binary, Str/binary>>.

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
        true ->
            <<>>;
        false ->
            <<A:8>>
    end;
rtrim(Text, Chars) ->
    Size = byte_size(Text) - 1,
    <<Rest:Size/binary, A:8>> = Text,
    case lists:member(A, Chars) of
        true ->
            rtrim(Rest, Chars);
        false ->
            Text
    end.

-spec ltrim(binary(), [byte()]) -> binary().
ltrim(<<>>, _Chars) ->
    <<>>;
ltrim(<<A:8>>, Chars) ->
    case lists:member(A, Chars) of
        true ->
            <<>>;
        false ->
            <<A:8>>
    end;
ltrim(<<A:8, Rest/binary>> = Text, Chars) ->
    case lists:member(A, Chars) of
        true ->
            ltrim(Rest, Chars);
        false ->
            Text
    end.

-spec join([binary()], binary()) -> binary().
join([], _Sep) ->
    <<>>;
join([Part], _Sep) ->
    Part;
join([Head | Tail], Sep) ->
    lists:foldl(fun(Value, Acc) -> <<Acc/binary, Sep/binary, Value/binary>> end, Head, Tail).

-spec vsn_cmp(binary(), binary()) -> integer();
             ([binary()], [binary()]) -> integer().
vsn_cmp(Vsn1, Vsn2) when is_binary(Vsn1) andalso is_binary(Vsn2) ->
    Vsn01 = hd(binary:split(Vsn1, <<"-">>)),
    Vsn1parts = binary:split(Vsn01, <<".">>, [global]),
    Vsn02 = hd(binary:split(Vsn2, <<"-">>)),
    Vsn2parts = binary:split(Vsn02, <<".">>, [global]),
    vsn_cmp([binary_to_integer(B) || B <- Vsn1parts],
            [binary_to_integer(B) || B <- Vsn2parts]);
vsn_cmp([], []) ->
    0;
vsn_cmp([_ | _], []) ->
    1;
vsn_cmp([], [_ | _]) ->
    -1;
vsn_cmp([A | _], [B | _]) when A > B ->
    1;
vsn_cmp([A | _], [B | _]) when A < B ->
    -1;
vsn_cmp([A | ARest], [A | BRest]) ->
    vsn_cmp(ARest, BRest).

-spec spaces(pos_integer()) -> binary().
%% @doc generate as many spaces as the number passed as param.
spaces(Num) ->
    repeat(Num, 32).

-spec repeat(pos_integer(), byte() | binary()) -> binary().
%% @doc repeat the byte passed as param as many times as the number passed as param.
repeat(Num, Byte) when is_integer(Byte) ->
    repeat(Num, Byte, <<>>);
repeat(Num, Bin) when is_binary(Bin) ->
    repeat(Num, Bin, <<>>).

-spec repeat(pos_integer(), byte() | binary(), binary()) -> binary().
%% @doc repeat internal function.
%% @see repeat/2
%% @private
repeat(0, _ByteOrBin, Binary) ->
    Binary;
repeat(N, Byte, Binary) when is_integer(Byte) ->
    repeat(N - 1, Byte, <<Binary/binary, Byte:8>>);
repeat(N, Bin, Binary) when is_binary(Bin) ->
    repeat(N - 1, Bin, <<Binary/binary, Bin/binary>>).

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
    to_lower(<< <<(integer_to_binary(X, 16))/binary>> || <<X:4/integer>> <= Bin >>).

-spec ibin2hex(binary()) -> binary().
%% @doc transform a binary string in its hexadecimal representation.
ibin2hex(Bin) when is_binary(Bin) ->
    to_lower(<< <<(integer_to_binary(Y, 16))/binary, (integer_to_binary(X, 16))/binary>>
                || <<X:4/integer, Y:4/integer>> <= Bin >>).

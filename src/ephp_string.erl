-module(ephp_string).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    to_lower/1,
    to_upper/1,
    escape/2,
    trim/1
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

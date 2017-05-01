-module(ephp_string).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    to_lower/1,
    to_upper/1,
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

-spec trim(binary()) -> binary();
          (undefined) -> undefined.

trim(undefined) ->
    undefined;

trim(Text) ->
    re:replace(Data, "^\\s+|\\s+$", "", [{return, binary}, global]).

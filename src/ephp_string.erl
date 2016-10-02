-module(ephp_string).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    to_lower/1
]).

-spec to_lower(binary()) -> binary().

to_lower(Text) ->
    unistring:to_lower(Text).

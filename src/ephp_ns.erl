%% @doc This module stores handle the way to use namespaces. The namespaces
%%      are needed mainly during parsing (compilation-time) but dynamically
%%      it's sometimes needed as well.
%% @end
-module(ephp_ns).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    normalize/1,
    join/2,
    to_bin/1,
    to_bin/2,
    parse/1
]).

-spec normalize(namespace()) -> namespace().
%% @doc normalize namespace removing initial empty (absolute) if any.
normalize([<<>>|NS]) -> NS;
normalize(NS) -> NS.

-spec join(namespace(), namespace()) -> namespace().
%% @doc join two namespaces to generate only one, checking for absolute and relative.
join(_BaseNS, [<<>>|RelativeNS]) -> RelativeNS;
join(BaseNS, RelativeNS) -> BaseNS ++ RelativeNS.

-spec to_bin(namespace()) -> binary().
%% @doc converts a namespace to the string representation.
to_bin(NS) ->
    ephp_string:join(NS, <<"\\">>).

-spec to_bin(namespace(), class_name()) -> binary().
%% @doc converts a namespace and class or function name to the string representation.
to_bin(NS, <<>>) ->
    to_bin(NS);
to_bin(NS, ClassName) ->
    to_bin(NS ++ [ClassName]).


-spec parse(binary()) -> {namespace(), class_name()}.
%% @doc converts a string into a 2-tuple: namespace and class name.
parse(Str) ->
    case binary:split(Str, <<"\\">>, [global]) of
        [ClassName] -> {[], ClassName};
        Parts -> {lists:droplast(Parts), lists:last(Parts)}
    end.

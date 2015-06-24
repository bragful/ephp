-module(ephp_func_array).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    in_array/3,
    count/2,
    sizeof/2
]).

-include("ephp.hrl").

-spec init() -> [ephp_func:php_function()].

init() -> [
    in_array,
    count,
    sizeof
]. 

-spec in_array(Context :: context(), Key :: var_value(), Array :: var_value()) -> boolean().

in_array(_Context, {_,Value}, {_,Array}) ->
    member(Value, Array).

-spec count(Context :: context(), Array :: var_value()) -> integer().

count(_Context, {_,Array}) when ?IS_DICT(Array) ->
    ?DICT:size(Array);

count(_Context, _Var) ->
    1.

-spec sizeof(Context :: context(), Array :: var_value()) -> integer().

sizeof(Context, Array) ->
    count(Context, Array).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

member(Value, Dict) ->
    List = ?DICT:to_list(Dict),
    lists:keysearch(Value, 2, List) =/= false.

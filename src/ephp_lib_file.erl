-module(ephp_lib_file).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    basename/3,
    dirname/3
]).

-include("ephp.hrl").

-spec init() -> [ephp_func:php_function()].

init() -> [
    basename, dirname
]. 

-spec basename(context(), line(), var_value()) -> binary().

basename(_Context, _Line, {_Var,PathFile}) ->
    filename:basename(PathFile).

-spec dirname(context(), line(), var_value()) -> binary().

dirname(_Context, _Line, {_Var,PathFile}) ->
    filename:dirname(PathFile).

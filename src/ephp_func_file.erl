-module(ephp_func_file).
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    basename/2,
    dirname/2
]).

-include("ephp.hrl").

-spec init() -> [ephp_func:php_function()].

init() -> [
    basename, dirname
]. 

-spec basename(context(), var_value()) -> binary().

basename(_Context, {_Var,PathFile}) ->
    filename:basename(PathFile).

-spec dirname(context(), var_value()) -> binary().

dirname(_Context, {_Var,PathFile}) ->
    filename:dirname(PathFile).

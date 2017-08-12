-module(ephp_lib_file).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    basename/3,
    dirname/3,
    file_exists/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    basename,
    dirname,
    file_exists
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec basename(context(), line(), var_value()) -> binary().

basename(_Context, _Line, {_Var, undefined}) ->
    <<>>;
basename(_Context, _Line, {_Var, PathFile}) ->
    filename:basename(ephp_data:to_bin(PathFile)).

-spec dirname(context(), line(), var_value()) -> binary().

dirname(_Context, _Line, {_Var, undefined}) ->
    <<>>;
dirname(_Context, _Line, {_Var, PathFile}) ->
    filename:dirname(ephp_data:to_bin(PathFile)).

-spec file_exists(context(), line(), var_value()) -> boolean().

file_exists(_Context, _Line, {_, Filename}) ->
    filelib:is_regular(Filename).

-module(ephp_lib_error).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    error_reporting/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    error_reporting
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec error_reporting(context(), line(), var_value()) -> integer().

error_reporting(Context, _Line, {_, ErrorLevel}) when is_integer(ErrorLevel) ->
    ephp_error:error_reporting(Context, ErrorLevel).

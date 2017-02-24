-module(ephp_lib_test).
-author('manuel@altenwald.com').
-compile([warnings_as_errors, export_all]).

-include("ephp.hrl").

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0
]).

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [
    {<<"test">>, <<"true">>}
].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [].

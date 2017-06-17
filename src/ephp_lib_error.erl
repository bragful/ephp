-module(ephp_lib_error).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,

    debug_backtrace/3,
    error_reporting/3,
    set_error_handler/4,
    restore_error_handler/2,
    error_get_last/2,
    error_clear_last/2
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {debug_backtrace, [
        {args, {0, 2, undefined, [{integer, 1}, {integer, 0}]}},
        pack_args
    ]},
    {set_error_handler, [
        {args, {2, 3, undefined, [mixed, {integer, ?E_ALL bor ?E_STRICT}]}}
    ]},
    restore_error_handler,
    error_get_last,
    error_clear_last,
    error_reporting
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [
    {<<"E_ERROR">>, ?E_ERROR},
    {<<"E_WARNING">>, ?E_WARNING},
    {<<"E_PARSE">>, ?E_PARSE},
    {<<"E_NOTICE">>, ?E_NOTICE},
    {<<"E_CORE_ERROR">>, ?E_CORE_ERROR},
    {<<"E_CORE_WARNING">>, ?E_CORE_WARNING},
    {<<"E_COMPILE_ERROR">>, ?E_COMPILE_ERROR},
    {<<"E_COMPILE_WARNING">>, ?E_COMPILE_WARNING},
    {<<"E_USER_ERROR">>, ?E_USER_ERROR},
    {<<"E_USER_WARNING">>, ?E_USER_WARNING},
    {<<"E_USER_NOTICE">>, ?E_USER_NOTICE},
    {<<"E_STRICT">>, ?E_STRICT},
    {<<"E_RECOVERABLE_ERROR">>, ?E_RECOVERABLE_ERROR},
    {<<"E_DEPRECATED">>, ?E_DEPRECATED},
    {<<"E_USER_DEPRECATED">>, ?E_USER_DEPRECATED},
    {<<"E_ALL">>, ?E_ALL},
    {<<"DEBUG_BACKTRACE_PROVIDE_OBJECT">>, 1},
    {<<"DEBUG_BACKTRACE_IGNORE_ARGS">>, 2}
].

-spec debug_backtrace(context(), line(), [var_value()]) -> ephp_array().

debug_backtrace(Context, _Line, [{_, _Flags}, {_, _Limit}]) ->
    ephp_stack:get_array(Context).

-spec error_reporting(context(), line(), var_value()) -> integer().

error_reporting(Context, _Line, {_, ErrorLevel}) when is_integer(ErrorLevel) ->
    ephp_error:error_reporting(Context, ErrorLevel).

-spec set_error_handler(context(), line(), var_value(), var_value()) -> mixed().

set_error_handler(Context, _Line, {_, ErrorHandler}, {_, ErrorLevel}) ->
    case ephp_error:get_error_handler_func(Context) of
        {OldErrorHandler, _} ->
            OldErrorHandler;
        undefined ->
            OldErrorHandler = undefined
    end,
    ephp_error:set_error_handler_func(Context, ErrorHandler, ErrorLevel),
    OldErrorHandler.

-spec restore_error_handler(context(), line()) -> true.

restore_error_handler(Context, _Line) ->
    ephp_error:remove_error_handler_func(Context),
    true.

-spec error_get_last(context(), line()) -> ephp_array() | undefined.

error_get_last(Context, _Line) ->
    ephp_error:get_last(Context).

-spec error_clear_last(context(), line()) -> undefined.

error_clear_last(Context, _Line) ->
    ephp_error:clear_last(Context),
    undefined.

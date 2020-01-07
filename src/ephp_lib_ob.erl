-module(ephp_lib_ob).
-author('manuel@altenwald.com').

-behaviour(ephp_lib).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    flush/2,
    ob_start/2,
    ob_start/3,
    ob_get_contents/2,
    ob_get_length/2,
    ob_clean/2
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    ob_start,
    flush,
    {flush, [{alias, <<"ob_flush">>}]},
    {flush, [{alias, <<"ob_end_flush">>}]},
    ob_get_contents,
    ob_get_length,
    {ob_get_length, [{alias, <<"ob_get_level">>}]},
    ob_clean,
    {ob_clean, [{alias, <<"ob_end_clean">>}]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [
    {<<"output_buffering">>, 4096},
    {<<"output_handler">>, undefined},
    {<<"implicit_flush">>, <<"On">>}
].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec flush(context(), line()) -> undefined.

flush(Context, _Line) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:flush(Output),
    undefined.

-spec ob_start(context(), line()) -> boolean().

ob_start(Context, _Line) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:flush(Output),
    ephp_output:set_flush(Output, false),
    ephp_output:set_output_handler(Output, undefined),
    true.

-spec ob_start(context(), line(), Callback :: var_value()) -> boolean().

ob_start(Context, Line, {_, Callback}) when is_binary(Callback) ->
    Output = ephp_context:get_output_handler(Context),
    PHPFunc = fun(Ctx, Var) ->
        Call = #call{
            line = Line,
            name = Callback,
            args = [Var]
        },
        ephp_context:solve(Ctx, Call)
    end,
    ephp_output:flush(Output),
    ephp_output:set_output_handler(Output, PHPFunc),
    true.

-spec ob_get_contents(context(), line()) -> binary().

ob_get_contents(Context, _Line) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:get(Output).

-spec ob_get_length(context(), line()) -> integer().

ob_get_length(Context, _Line) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:size(Output).

-spec ob_clean(context(), line()) -> undefined.

ob_clean(Context, _Line) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:pop(Output),
    undefined.

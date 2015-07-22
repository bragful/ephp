-module(ephp_lib_ob).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    flush/2,
    ob_start/2,
    ob_start/3,
    ob_get_contents/2,
    ob_get_length/2,
    ob_clean/2
]).

-include("ephp.hrl").

-spec init() -> [ephp_func:php_function()].

init() -> [
    flush,
    ob_start,
    {flush, <<"ob_flush">>},
    ob_get_contents,
    ob_get_length,
    {flush, <<"ob_end_flush">>},
    ob_clean,
    {ob_clean, <<"ob_end_clean">>}
]. 

-spec flush(context(), line()) -> null.

flush(Context, _Line) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:flush(Output),
    null.

-spec ob_start(context(), line()) -> boolean().

ob_start(Context, _Line) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:flush(Output),
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

-spec ob_clean(context(), line()) -> null.

ob_clean(Context, _Line) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:pop(Output),
    null.

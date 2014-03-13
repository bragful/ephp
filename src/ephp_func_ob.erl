-module(ephp_func_ob).
-compile([warnings_as_errors]).

-export([
    init/1,
    flush/1,
    ob_start/1,
    ob_flush/1,
    ob_end_flush/1,
    ob_get_contents/1,
    ob_get_length/1,
    ob_clean/1,
    ob_end_clean/1
]).

-include("ephp.hrl").

-spec init(Context :: context()) -> ok.

init(Context) ->
    Funcs = [
        flush, ob_start, ob_flush, ob_get_contents,
        ob_get_length, ob_end_flush, ob_clean,
        ob_end_clean
    ],
    lists:foreach(fun(Func) ->
        Name = atom_to_binary(Func, utf8),
        ephp_context:register_func(Context, Name, ?MODULE, Func)  
    end, Funcs), 
    ok. 

-spec flush(Context :: context()) -> null.

flush(Context) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:flush(Output),
    null.

-spec ob_start(Context :: context()) -> boolean().

ob_start(Context) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:set_flush(Output, true),
    ephp_output:flush(Output),
    true. 

-spec ob_flush(Context :: context()) -> null.

ob_flush(Context) ->
    flush(Context).

-spec ob_end_flush(Context :: context()) -> null.

ob_end_flush(Context) ->
    flush(Context).

-spec ob_get_contents(Context :: context()) -> binary().

ob_get_contents(Context) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:get(Output).

-spec ob_get_length(Context :: context()) -> integer().

ob_get_length(Context) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:size(Output).

-spec ob_clean(Context :: context()) -> null.

ob_clean(Context) ->
    Output = ephp_context:get_output_handler(Context),
    ephp_output:pop(Output),
    null.

-spec ob_end_clean(Context :: context()) -> null.

ob_end_clean(Context) ->
    ob_clean(Context).

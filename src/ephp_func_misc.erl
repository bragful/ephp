-module(ephp_func_misc).
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    define/3,
    sleep/2,
    usleep/2,
    exit/2,
    shutdown/1
]).

-include("ephp.hrl").

-spec init() -> [ephp_func:php_function()].

init() -> [
    define,
    sleep,
    usleep,
    {exit, <<"die">>},
    exit,
    {shutdown, <<"__do_shutdown">>}
]. 

-spec define(context(), Constant :: var_value(), 
    Content :: var_value()) -> boolean().

define(Context, {#constant{name=Constant},_}, {_UnParsedContent,Content}) ->
    ephp_context:register_const(Context, Constant, Content),
    true.

-spec sleep(context(), Seconds :: var_value()) -> false | integer().

sleep(_Context, {_, Seconds}) when is_number(Seconds) ->
    timer:sleep(trunc(Seconds) * 1000),
    0;

sleep(_Context, _) ->
    false.

-spec usleep(context(), MicroSeconds :: var_value()) -> 
    false | integer().

usleep(_Context, {_, MicroSeconds}) when is_number(MicroSeconds) ->
    timer:sleep(trunc(MicroSeconds) div 1000),
    0;

usleep(_Context, _) ->
    false.

-spec exit(context(), Message :: var_value()) ->
    null.

exit(Context, {_, Value}) ->
    ephp_context:set_output(Context, Value),
    throw(die).

-spec shutdown(context()) -> null.

shutdown(Context) ->
    Result = lists:foldl(fun
        (FuncName, false) ->
            Shutdown = #call{name = FuncName},
            ephp_interpr:run(Context, #eval{statements=[Shutdown]});
        (_, Break) ->
            Break
    end, false, ephp_context:get_shutdown_funcs(Context)),
    if Result =:= false ->
        ?DICT:fold(fun(K,V,Acc) ->
            ephp_func_vars:unset(Context, {#variable{name=K},V}),
            Acc
        end, null, ephp_context:get(Context, #variable{name = <<"GLOBALS">>}));
    true ->
        null
    end.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------


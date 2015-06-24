-module(ephp_func_misc).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    define/4,
    sleep/3,
    usleep/3,
    exit/3,
    shutdown/1
]).

-include("ephp.hrl").

-spec init() -> [ephp_func:php_function()].

init() -> [
    define,
    sleep,
    usleep,
    {exit, <<"die">>},
    exit
]. 

-spec define(context(), line(), Constant :: var_value(), 
    Content :: var_value()) -> boolean().

define(Context, _Line, {#constant{name=Constant},_},
        {_UnParsedContent,Content}) ->
    ephp_context:register_const(Context, Constant, Content),
    true.

-spec sleep(context(), line(), Seconds :: var_value()) -> false | integer().

sleep(_Context, _Line, {_, Seconds}) when is_number(Seconds) ->
    timer:sleep(trunc(Seconds) * 1000),
    0;

sleep(_Context, _Line, _) ->
    false.

-spec usleep(context(), line(), MicroSeconds :: var_value()) -> 
    false | integer().

usleep(_Context, _Line, {_, MicroSeconds}) when is_number(MicroSeconds) ->
    timer:sleep(trunc(MicroSeconds) div 1000),
    0;

usleep(_Context, _Line, _) ->
    false.

-spec exit(context(), line(), Message :: var_value()) ->
    null.

exit(Context, _Line, {_, Value}) ->
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
            ephp_func_vars:unset(Context, undefined, {#variable{name=K},V}),
            Acc
        end, null, ephp_context:get(Context, #variable{name = <<"GLOBALS">>}));
    true ->
        null
    end.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------


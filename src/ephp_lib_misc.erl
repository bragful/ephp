-module(ephp_lib_misc).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    define/4,
    defined/3,
    sleep/3,
    usleep/3,
    exit/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    define,
    defined,
    sleep,
    usleep,
    {exit, [{alias, <<"die">>}]},
    exit
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec define(context(), line(), Constant :: var_value(),
    Content :: var_value()) -> boolean().

define(Context, _Line, {#text{},Constant}, {_,Content}) ->
    ephp_context:register_const(Context, Constant, Content),
    true;

define(Context, _Line, {#constant{name=Constant},_},
        {_UnParsedContent,Content}) ->
    ephp_context:register_const(Context, Constant, Content),
    true.

-spec defined(context(), line(), var_value()) -> boolean().

defined(Context, _Line, {_, ConstantName}) ->
    case ephp_context:get_const(Context, ConstantName, false) of
        false -> false;
        _ -> true
    end.

-spec sleep(context(), line(), Seconds :: var_value()) -> false | integer().

sleep(_Context, _Line, {_, Seconds}) when is_number(Seconds) ->
    timer:sleep(trunc(Seconds) * 1000),
    0;

sleep(Context, Line, {_, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"sleep">>, 1, <<"long">>, ephp_data:gettype(Val)},
    ephp_error:handle_error(Context, {error, ewrongarg, Line, File,
        ?E_WARNING, Data}),
    false.

-spec usleep(context(), line(), MicroSeconds :: var_value()) ->
    false | integer().

usleep(_Context, _Line, {_, MicroSeconds}) when is_number(MicroSeconds) ->
    timer:sleep(trunc(MicroSeconds) div 1000),
    0;

usleep(Context, Line, {_, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"usleep">>, 1, <<"long">>, ephp_data:gettype(Val)},
    ephp_error:handle_error(Context, {error, ewrongarg, Line, File,
        ?E_WARNING, Data}),
    false.

-spec exit(context(), line(), Message :: var_value()) ->
    undefined.

exit(Context, _Line, {_, Value}) ->
    ephp_context:set_output(Context, Value),
    throw(die).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------


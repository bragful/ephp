-module(ephp_lib_class).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    get_class/3,
    class_alias/4
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {get_class, [{args, {1, false, [object]}}]},
    class_alias
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec get_class(context(), line(), Class :: var_value()) -> any().

get_class(_Context, _Line, {_,#reg_instance{class=#class{name=ClassName}}}) ->
    ClassName.

-spec class_alias(context(), line(),
                  ClassName :: var_value(),
                  ClassAlias :: var_value()) -> boolean().

class_alias(Context, Line, {_,Name}, {_,Alias}) ->
    case ephp_context:set_class_alias(Context, Name, Alias) of
        ok ->
            true;
        {error, enoexist} ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefclass, Line, ?E_WARNING, {File, Name}}),
            false;
        {error, eredefined} ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eredefinedclass, Line, ?E_ERROR, {File, Alias}}),
            false
    end.

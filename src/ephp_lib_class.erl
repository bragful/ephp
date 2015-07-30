-module(ephp_lib_class).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    get_class/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    get_class
]. 

-spec get_class(context(), line(), Class :: var_value()) -> any().

get_class(_Context, _Line, {_,#reg_instance{class=#class{name=ClassName}}}) ->
    ClassName.

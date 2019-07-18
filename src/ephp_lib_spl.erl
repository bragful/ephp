-module(ephp_lib_spl).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    spl_autoload_call/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {spl_autoload_call, [{args, [string]}]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec spl_autoload_call(context(), line(), var_value()) -> undefined.

spl_autoload_call(Context, Line, {_, RawClassName}) ->
    {ClassNS, ClassName} = ephp_class:str2ns(RawClassName),
    case ephp_class:get(Context, ClassNS, ClassName, spl) of
        {ok, _Class} ->
            undefined;
        {error, enoexist} ->
            ephp_stack:push(Context, undefined, Line, <<"spl_autoload">>,
                            [ClassName], undefined, undefined),
            Classes = ephp_context:get_classes(Context),
            ExceptionName = <<"LogicException">>,
            Exception = ephp_class:instance(Classes, Context, Context,
                                            [], ExceptionName, Line),
            #ephp_object{class = Class} = ephp_object:get(Exception),
            #class_method{name = ConstructorName} =
                ephp_class:get_constructor(Classes, Class),
            Call = #call{type = object,
                         name = ConstructorName,
                         args = [<<"Class ", ClassName/binary,
                                   " could not be loaded">>],
                         line = Line},
            ephp_context:call_method(Context, Exception, Call),
            File = ephp_context:get_active_file(Context),
            Data = {File, ephp_error:get_line(Line), Exception},
            ephp_error:error({error, euncaught, Line, ?E_ERROR, Data})
    end.

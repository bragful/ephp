-module(ephp_lib_func).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    register_shutdown_function/3,
    get_defined_functions/2,
    function_exists/3,
    func_num_args/2
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {register_shutdown_function, [pack_args]},
    get_defined_functions,
    function_exists,
    func_num_args
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec register_shutdown_function(context(), line(), [var_value()]) -> ok.

register_shutdown_function(Context, _Line, [{_,Callback}|_RawArgs]) ->
    %% TODO: add params to call the functions.
    ephp_context:register_shutdown_func(Context, Callback),
    ok.

-spec func_num_args(context(), line()) -> non_neg_integer().

func_num_args(Context, _Line) ->
    ephp_context:get_active_function_arity(Context).

-spec get_defined_functions(context(), line()) -> ephp_array().

get_defined_functions(Context, _Line) ->
    Append = fun({Func,Type}, {I,Dict}) ->
        NewTypeDict = case ephp_array:find(Type, Dict) of
            {ok,TypeDict} ->
                ephp_array:store(I, Func, TypeDict)
        end,
        {I+1,ephp_array:store(Type, NewTypeDict, Dict)}
    end,
    BaseDict = ephp_array:store(<<"user">>, ephp_array:new(),
        ephp_array:store(<<"internal">>, ephp_array:new(), ephp_array:new())),
    Functions = ephp_context:get_functions(Context),
    {_,FuncList} = lists:foldl(Append, {0,BaseDict}, Functions),
    FuncList.

-spec function_exists(context(), line(), FuncName :: var_value()) -> boolean().

function_exists(Context, _Line, {_,FuncName}) ->
    ephp_context:get_function(Context, FuncName) =/= error.

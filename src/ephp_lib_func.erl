-module(ephp_lib_func).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    register_shutdown_function/3,
    get_defined_functions/2,
    function_exists/3,
    func_num_args/2
]).

-include("ephp.hrl").

-spec init() -> [
    ephp_func:php_function() |
    {ephp_func:php_function(), ephp_func:php_function_alias()}
].

init() -> [
    {register_shutdown_function, [pack_args]},
    get_defined_functions,
    function_exists,
    func_num_args
].

-spec register_shutdown_function(context(), line(), [var_value()]) -> ok.

register_shutdown_function(Context, _Line, [{_,Callback}|_RawArgs]) ->
    %% TODO: add params to call the functions.
    ephp_context:register_shutdown_func(Context, Callback),
    ok.

-spec func_num_args(context(), line()) -> non_neg_integer().

func_num_args(Context, _Line) ->
    ephp_context:get_current_function_arity(Context).

-spec get_defined_functions(context(), line()) -> ?DICT_TYPE.

get_defined_functions(Context, _Line) ->
    Append = fun(Type, I, Func, Dict) ->
        NewTypeDict = case ?DICT:find(Type, Dict) of
            {ok,TypeDict} ->
                ?DICT:store(I, Func, TypeDict);
            error ->
                ?DICT:store(I, Func, ?DICT:new())
        end,
        ?DICT:store(Type, NewTypeDict, Dict)
    end,
    BaseDict = ?DICT:store(<<"user">>, ?DICT:new(),
        ?DICT:store(<<"internal">>, ?DICT:new(), ?DICT:new())),
    {_,FuncList} = lists:foldl(fun
        ({<<"__",_/binary>>,<<"internal">>}, Result) ->
            Result;
        ({Func,Type}, {I,FL}) ->
            {I+1,Append(Type,I,Func,FL)}
    end, {0,BaseDict}, ephp_context:get_functions(Context)),
    FuncList.

-spec function_exists(context(), line(), FuncName :: var_value()) -> boolean().

function_exists(Context, _Line, {_,FuncName}) ->
    ephp_context:get_function(Context, FuncName) =/= error.

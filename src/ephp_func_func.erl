-module(ephp_func_func).
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    register_shutdown_function/2,
    get_defined_functions/1,
    function_exists/2
]).

-include("ephp.hrl").

-spec init() -> [
    ephp_func:php_function() |
    {ephp_func:php_function(), ephp_func:php_function_alias()}
].

init() -> [
    {register_shutdown_function, true},
    get_defined_functions,
    function_exists
].

register_shutdown_function(Context, [{_,Callback}|_RawArgs]) ->
    %% TODO: add params to call the functions.
    ephp_context:register_shutdown_func(Context, Callback),
    ok.

get_defined_functions(Context) ->
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

function_exists(Context, {_,FuncName}) ->
    ephp_context:get_function(Context, FuncName) =/= error.

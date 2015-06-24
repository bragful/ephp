-module(ephp_func).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-type php_function() :: atom().
-type php_function_alias() :: binary().

-export_type([
    php_function/0,
    php_function_alias/0
]).

-callback init() -> [php_function()].

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    destroy/1,

    get/2,
    get_functions/1,

    run/2,

    register_func/3,
    register_func/4,
    register_func/5
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, ?DICT:new()),
    {ok, Ref}.

destroy(Funcs) ->
    erlang:erase(Funcs).

get(Ref, FuncName) ->
    Funcs = erlang:get(Ref),
    IFuncName = ephp_util:to_lower(FuncName),
    ?DICT:find(IFuncName, Funcs).

get_functions(Ref) ->
    Funcs = erlang:get(Ref),
    Get = fun
        (#reg_func{name=Name, type=builtin}) -> {Name, <<"internal">>};
        (#reg_func{name=Name, type=php}) -> {Name, <<"user">>}
    end,
    [ Get(FuncName) || {_,FuncName} <- ?DICT:to_list(Funcs) ].

register_func(Ref, PHPFunc, Module, Fun)
        when is_atom(Module) andalso is_atom(Fun) ->
    register_func(Ref, PHPFunc, Module, Fun, false);

register_func(Ref, PHPFunc, Fun, PackArgs) when is_function(Fun) -> 
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_util:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=builtin,
        builtin=Fun,
        pack_args=PackArgs},
    erlang:put(Ref, ?DICT:store(IPHPFunc, RegFunc, Funcs)),
    ok;

register_func(Ref, PHPFunc, Args, Code) ->
    register_func(Ref, PHPFunc, Args, Code, false).

register_func(Ref, PHPFunc, Fun) ->
    register_func(Ref, PHPFunc, Fun, false).

register_func(Ref, PHPFunc, Module, Fun, PackArgs)
        when is_atom(Module) andalso is_atom(Fun) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_util:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=builtin,
        builtin={Module, Fun},
        pack_args=PackArgs},
    erlang:put(Ref, ?DICT:store(IPHPFunc, RegFunc, Funcs)),
    ok;

register_func(Ref, PHPFunc, Args, Code, PackArgs) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_util:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=php,
        args=Args,
        code=Code,
        pack_args=PackArgs},
    erlang:put(Ref, ?DICT:store(IPHPFunc, RegFunc, Funcs)),
    ok.


run(Context, #call{line=Line}=Call) ->
    try 
        ephp_context:solve(Context, Call),
        false
    catch
        throw:die ->
            {return, null};
        throw:{error, erequired, _, ReqFile} ->
            File = ephp_context:get_active_file(Context),
            Data = {File, ReqFile},
            ephp_error:handle_error(Context, {error, erequired, Line, Data});
        throw:{error, eundefun, _, Fun} ->
            File = ephp_context:get_active_file(Context),
            Data = {File, Fun},
            ephp_error:handle_error(Context, {error, eundefun, Line, Data})
    end.

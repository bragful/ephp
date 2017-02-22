-module(ephp_func).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-type php_function() :: atom().
-type php_function_opt() :: atom() | {atom(), any()}.
-type php_function_opts() :: [php_function_opts()].
-type php_function_result() ::
    php_function() | {php_function(), php_function_opts}.
-type php_function_results() :: [php_function_result()].

-type config_section() :: binary().
-type config_param() :: binary().
-type php_config_results() :: [{config_section(), [config_param()]}].

-type const_name() :: binary().
-type php_const_results() :: [{const_name(), float() | integer() | binary()}].

-export_type([
    php_function/0,
    php_function_results/0,
    php_function_result/0,
    php_function_opts/0,
    php_function_opt/0,

    config_section/0,
    config_param/0,
    php_config_results/0
]).

-callback init_func() -> php_function_results().

-callback init_config() -> php_config_results().

-callback init_const() -> php_const_results().

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
    erlang:put(Ref, dict:new()),
    {ok, Ref}.

destroy(Funcs) ->
    erlang:erase(Funcs).

get(Ref, FuncName) ->
    Funcs = erlang:get(Ref),
    IFuncName = ephp_string:to_lower(FuncName),
    dict:find(IFuncName, Funcs).

get_functions(Ref) ->
    Funcs = erlang:get(Ref),
    Get = fun
        (#reg_func{name=Name, type=builtin}) -> {Name, <<"internal">>};
        (#reg_func{name=Name, type=php}) -> {Name, <<"user">>}
    end,
    [ Get(FuncName) || {_,FuncName} <- dict:to_list(Funcs) ].

register_func(Ref, PHPFunc, Module, Fun)
        when is_atom(Module) andalso is_atom(Fun) ->
    register_func(Ref, PHPFunc, Module, Fun, false);

register_func(Ref, PHPFunc, Fun, PackArgs) when is_function(Fun) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_string:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=builtin,
        builtin=Fun,
        pack_args=PackArgs},
    erlang:put(Ref, dict:store(IPHPFunc, RegFunc, Funcs)),
    ok;

register_func(Ref, PHPFunc, Args, Code) ->
    register_func(Ref, PHPFunc, Args, Code, false).

register_func(Ref, PHPFunc, Fun) ->
    register_func(Ref, PHPFunc, Fun, false).

register_func(Ref, PHPFunc, Module, Fun, PackArgs)
        when is_atom(Module) andalso is_atom(Fun) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_string:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=builtin,
        builtin={Module, Fun},
        pack_args=PackArgs},
    erlang:put(Ref, dict:store(IPHPFunc, RegFunc, Funcs)),
    ok;

register_func(Ref, PHPFunc, Args, Code, PackArgs) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_string:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=php,
        args=Args,
        code=Code,
        pack_args=PackArgs},
    erlang:put(Ref, dict:store(IPHPFunc, RegFunc, Funcs)),
    ok.


run(Context, #call{line=Line}=Call) ->
    try
        ephp_context:solve(Context, Call),
        false
    catch
        throw:die ->
            {return, undefined};
        throw:{error, erequired, _, ReqFile} ->
            File = ephp_context:get_active_file(Context),
            Data = {File, ReqFile},
            ephp_error:handle_error(Context, {error, erequired, Line,
                ?E_ERROR, Data});
        throw:{error, eundefun, _, Fun} ->
            File = ephp_context:get_active_file(Context),
            Data = {File, Fun},
            ephp_error:handle_error(Context, {error, eundefun, Line,
                ?E_ERROR, Data})
    end.

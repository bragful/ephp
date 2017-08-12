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
-type config_param() :: mixed().
-type php_config_results() :: [{config_section(), [config_param()]}].

-type const_name() :: binary().
-type php_const_results() :: [{const_name(), float() | integer() | binary()}].

-type default_value() :: mixed().
-type error_return_value() :: mixed().
-type min_args() :: non_neg_integer().
-type max_args() :: non_neg_integer().
-type type_arg() :: mixed |
                    string |
                    integer |
                    array |
                    object |
                    resource |
                    raw |
                    type_ref.
-type validation_arg() ::
    {type_arg(), default_value()} |
    type_arg().
-type validation_args() ::
    {min_args(), max_args(), error_return_value(), [validation_arg()]} |
    [validation_arg()] |
    undefined |
    no_resolve.

-export_type([
    php_function/0,
    php_function_results/0,
    php_function_result/0,
    php_function_opts/0,
    php_function_opt/0,

    config_section/0,
    config_param/0,
    php_config_results/0,
    validation_args/0
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
    is_defined/2,
    get_functions/1,

    get_static_value/3,
    set_static_value/4,
    set_static/3,
    init_static_value/4,

    run/2,

    register_func/5,
    register_func/6,
    register_func/7
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, dict:new()),
    {ok, Ref}.

destroy(Funcs) ->
    erlang:erase(Funcs),
    ok.

get(Ref, FuncName) ->
    Funcs = erlang:get(Ref),
    IFuncName = ephp_string:to_lower(FuncName),
    dict:find(IFuncName, Funcs).

get_static_value(Ref, FuncName, VarName) ->
    case get(Ref, FuncName) of
        {ok, #reg_func{static = Static}} ->
            case orddict:find(VarName, Static) of
                {ok, Value} -> Value;
                error -> undefined
            end;
        error ->
            throw({error, enofunc})
    end.

set_static(Ref, FuncName, Vars) ->
    Funcs = erlang:get(Ref),
    IFuncName = ephp_string:to_lower(FuncName),
    {ok, #reg_func{static = Static} = RegFunc} = dict:find(IFuncName, Funcs),
    NewStatic = lists:map(fun({Key, _}) ->
        %% TODO check behaviour when use unset
        NewValue = ephp_vars:get(Vars, #variable{name = Key}),
        {Key, NewValue}
    end, Static),
    NewRegFunc = RegFunc#reg_func{static = NewStatic},
    NewFuncs = dict:store(IFuncName, NewRegFunc, Funcs),
    erlang:put(Ref, NewFuncs),
    ok.

set_static_value(Ref, FuncName, VarName, Value) ->
    Funcs = erlang:get(Ref),
    IFuncName = ephp_string:to_lower(FuncName),
    case dict:find(IFuncName, Funcs) of
        {ok, #reg_func{static = Static} = RegFunc} ->
            NewStatic = orddict:store(VarName, Value, Static),
            NewRegFunc = RegFunc#reg_func{static = NewStatic},
            NewFuncs = dict:store(IFuncName, NewRegFunc, Funcs),
            erlang:put(Ref, NewFuncs),
            ok;
        error ->
            throw({error, enofunc})
    end.

init_static_value(Ref, FuncName, VarName, Value) ->
    Funcs = erlang:get(Ref),
    IFuncName = ephp_string:to_lower(FuncName),
    case dict:find(IFuncName, Funcs) of
        {ok, #reg_func{static = Static}} ->
            case orddict:find(VarName, Static) of
                {ok, RealValue} ->
                    RealValue;
                error ->
                    UpdateFunc = fun(RegFunc) ->
                        NewStatic = orddict:store(VarName, Value, Static),
                        RegFunc#reg_func{static = NewStatic}
                    end,
                    NewFuncs = dict:update(IFuncName, UpdateFunc, Funcs),
                    erlang:put(Ref, NewFuncs),
                    Value
            end;
        error ->
            throw({error, enofunc})
    end.

is_defined(Ref, FuncName) ->
    Funcs = erlang:get(Ref),
    dict:is_key(FuncName, Funcs).

get_functions(Ref) ->
    Funcs = erlang:get(Ref),
    Get = fun
        (#reg_func{name=Name, type=builtin}) -> {Name, <<"internal">>};
        (#reg_func{name=Name, type=php}) -> {Name, <<"user">>}
    end,
    [ Get(FuncName) || {_,FuncName} <- dict:to_list(Funcs) ].

register_func(Ref, File, PHPFunc, Args, Code) ->
    register_func(Ref, File, PHPFunc, Args, Code, false, undefined).

register_func(Ref, File, PHPFunc, Module, Fun, ValArgs)
        when is_atom(Module) andalso is_atom(Fun) ->
    register_func(Ref, File, PHPFunc, Module, Fun, false, ValArgs).

register_func(Ref, File, PHPFunc, Module, Fun, PackArgs, ValArgs)
        when is_atom(Module) andalso is_atom(Fun) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_string:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=builtin,
        file=File,
        builtin={Module, Fun},
        pack_args=PackArgs,
        validation_args=ValArgs},
    erlang:put(Ref, dict:store(IPHPFunc, RegFunc, Funcs)),
    ok;

register_func(Ref, File, PHPFunc, Args, Code, PackArgs, ValArgs) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_string:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=php,
        file=File,
        args=Args,
        code=Code,
        pack_args=PackArgs,
        validation_args=ValArgs},
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
            Data = {ReqFile},
            ephp_error:handle_error(Context, {error, erequired, Line, File,
                ?E_ERROR, Data});
        throw:{error, eundefun, _, Fun} ->
            File = ephp_context:get_active_file(Context),
            Data = {Fun},
            ephp_error:handle_error(Context, {error, eundefun, Line, File,
                ?E_ERROR, Data})
    end.

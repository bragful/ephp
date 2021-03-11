-module(ephp_func).

-author('manuel@altenwald.com').

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, destroy/1, get/2, get/3, is_defined/2, is_defined/3,
         get_functions/1, get_static_value/3, get_static_value/4, set_static_value/4,
         set_static_value/5, set_static/4, set_static/5, init_static_value/4, init_static_value/5,
         run/2, register_func/6, register_func/7, register_func/8]).

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
    get(Ref, [], FuncName).

get(Ref, NameSpace, FuncName) ->
    Funcs = erlang:get(Ref),
    IFuncName = ephp_string:to_lower(FuncName),
    dict:find({NameSpace, IFuncName}, Funcs).

get_static_value(Ref, FuncName, VarName) ->
    get_static_value(Ref, [], FuncName, VarName).

get_static_value(Ref, NameSpace, FuncName, VarName) ->
    case get(Ref, NameSpace, FuncName) of
        {ok, #reg_func{static = Static}} ->
            case orddict:find(VarName, Static) of
                {ok, Value} ->
                    Value;
                error ->
                    undefined
            end;
        error ->
            throw({error, enofunc})
    end.

set_static(Ref, FuncName, Vars, Context) ->
    set_static(Ref, [], FuncName, Vars, Context).

set_static(Ref, NameSpace, FuncName, Vars, Context) ->
    Funcs = erlang:get(Ref),
    FullFuncName = {NameSpace, ephp_string:to_lower(FuncName)},
    {ok, #reg_func{static = Static} = RegFunc} = dict:find(FullFuncName, Funcs),
    NewStatic =
        lists:map(fun({Key, _}) ->
                     %% TODO check behaviour when use unset
                     NewValue = ephp_vars:get(Vars, #variable{name = Key}, Context),
                     {Key, NewValue}
                  end,
                  Static),
    NewRegFunc = RegFunc#reg_func{static = NewStatic},
    NewFuncs = dict:store(FullFuncName, NewRegFunc, Funcs),
    erlang:put(Ref, NewFuncs),
    ok.

set_static_value(Ref, FuncName, VarName, Value) ->
    set_static_value(Ref, [], FuncName, VarName, Value).

set_static_value(Ref, NameSpace, FuncName, VarName, Value) ->
    Funcs = erlang:get(Ref),
    FullFuncName = {NameSpace, ephp_string:to_lower(FuncName)},
    case dict:find(FullFuncName, Funcs) of
        {ok, #reg_func{static = Static} = RegFunc} ->
            NewStatic = orddict:store(VarName, Value, Static),
            NewRegFunc = RegFunc#reg_func{static = NewStatic},
            NewFuncs = dict:store(FullFuncName, NewRegFunc, Funcs),
            erlang:put(Ref, NewFuncs),
            ok;
        error ->
            throw({error, enofunc})
    end.

init_static_value(Ref, FuncName, VarName, Value) ->
    init_static_value(Ref, [], FuncName, VarName, Value).

init_static_value(Ref, NameSpace, FuncName, VarName, Value) ->
    Funcs = erlang:get(Ref),
    FullFuncName = {NameSpace, ephp_string:to_lower(FuncName)},
    case dict:find(FullFuncName, Funcs) of
        {ok, #reg_func{static = Static}} ->
            case orddict:find(VarName, Static) of
                {ok, RealValue} ->
                    RealValue;
                error ->
                    UpdateFunc =
                        fun(RegFunc) ->
                           NewStatic = orddict:store(VarName, Value, Static),
                           RegFunc#reg_func{static = NewStatic}
                        end,
                    NewFuncs = dict:update(FullFuncName, UpdateFunc, Funcs),
                    erlang:put(Ref, NewFuncs),
                    Value
            end;
        error ->
            throw({error, enofunc})
    end.

is_defined(Ref, FuncName) ->
    is_defined(Ref, [], FuncName).

is_defined(Ref, NameSpace, FuncName) ->
    Funcs = erlang:get(Ref),
    FullFuncName = {NameSpace, FuncName},
    dict:is_key(FullFuncName, Funcs).

get_functions(Ref) ->
    Funcs = erlang:get(Ref),
    %% TODO check if NS is applied here
    Get = fun (#reg_func{name = Name, type = builtin}) ->
                  {Name, <<"internal">>};
              (#reg_func{name = Name, type = php}) ->
                  {Name, <<"user">>}
          end,
    [Get(FuncData) || {_, FuncData} <- dict:to_list(Funcs)].

register_func(Ref, File, NS, PHPFunc, Args, Code) ->
    register_func(Ref, File, NS, PHPFunc, Args, Code, false, undefined).

register_func(Ref, File, NS, PHPFunc, Module, Fun, ValArgs)
    when is_atom(Module) andalso is_atom(Fun) ->
    register_func(Ref, File, NS, PHPFunc, Module, Fun, false, ValArgs).

register_func(Ref, File, NS, PHPFunc, Module, Fun, PackArgs, ValArgs)
    when is_atom(Module) andalso is_atom(Fun) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_string:to_lower(PHPFunc),
    FullPHPFunc = {NS, IPHPFunc},
    RegFunc =
        #reg_func{name = IPHPFunc,
                  type = builtin,
                  file = File,
                  builtin = {Module, Fun},
                  pack_args = PackArgs,
                  validation_args = ValArgs},
    erlang:put(Ref, dict:store(FullPHPFunc, RegFunc, Funcs)),
    ok;
register_func(Ref, File, NS, PHPFunc, Args, Code, PackArgs, ValArgs) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_string:to_lower(PHPFunc),
    FullPHPFunc = {NS, IPHPFunc},
    RegFunc =
        #reg_func{name = IPHPFunc,
                  type = php,
                  file = File,
                  args = Args,
                  code = Code,
                  pack_args = PackArgs,
                  validation_args = ValArgs},
    erlang:put(Ref, dict:store(FullPHPFunc, RegFunc, Funcs)),
    ok.

run(Context, #call{line = Line, namespace = NS} = Call) ->
    try
        ephp_context:solve(Context, Call),
        false
    catch
        {error, erequired, _, ReqFile} ->
            File = ephp_context:get_active_file(Context),
            Data = {ReqFile},
            ephp_error:handle_error(Context, {error, erequired, Line, File, ?E_ERROR, Data});
        {error, eundefun, _, Fun} ->
            File = ephp_context:get_active_file(Context),
            Data = {NS, Fun},
            ephp_error:handle_error(Context, {error, eundefun, Line, File, ?E_ERROR, Data})
    end.

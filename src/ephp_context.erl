-module(ephp_context).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-record(state, {
    ref :: ephp:context_id() | undefined,
    vars :: ephp:vars_id() | undefined,
    funcs :: ephp:funcs_id(),
    class :: ephp:classes_id(),
    object :: ephp:objects_id(),
    output :: ephp:output_id(),
    const :: ephp:consts_id(),
    global :: ephp:context_id() | undefined,
    include :: ephp:includes_id(),
    shutdown :: ephp:shutdown_id(),
    errors :: ephp:errors_id(),
    meta = [] :: term(),

    active_file = <<>> :: file_name(),
    active_fun = <<>> :: function_name(),
    active_fun_ns = [] :: namespace(),
    active_fun_args = 0 :: non_neg_integer(),
    active_class = <<>> :: class_name(),
    active_class_ns = [] :: namespace(),
    active_real_class = <<>> :: class_name(),
    active_real_class_ns = [] :: namespace()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    clone/1,
    get/2,
    set/3,
    set_bulk/2,
    del/2,
    isset/2,
    empty/2,
    solve/2,
    destroy/1,
    destroy_all/1,

    get_vars/1,
    get_consts/1,
    get_objects/1,
    get_classes/1,
    get_funcs/1,

    get_active_file/1,
    set_active_file/2,
    get_active_class/1,
    set_active_class/3,
    get_active_class_ns/1,
    get_active_real_class/1,
    set_active_real_class/3,
    get_active_real_class_ns/1,

    get_output/1,
    set_output/2,
    set_output_handler/2,
    get_output_handler/1,

    call_function/2,
    register_func/5,
    register_func/6,
    register_func/7,
    get_functions/1,
    get_function/2,
    is_defined_function/2,

    get_active_function/1,
    get_active_function_ns/1,
    get_active_function_arity/1,

    set_errors_id/2,
    get_errors_id/1,

    is_const_defined/4,
    get_const/3,
    get_const/4,
    get_const/5,
    register_const/3,
    register_const/4,

    load/2,
    load_once/2,

    call_method/3,
    register_class/2,
    register_interface/2,
    set_class_alias/5,

    set_global/2,
    get_global/1,
    generate_subcontext/1,
    generate_subcontext/2,

    get_meta/2,
    set_meta/3,

    register_shutdown_func/2,
    unregister_shutdown_func/2,
    get_shutdown_funcs/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    %% TODO: remove all of the concrete parts like funcs, output, const,
    %%       include, objects, errors, classes, shutdown... keep only vars
    %%       because the other elements are common for the context and
    %%       subcontexts. Finally MUST exist only one context per PID.
    Ref = make_ref(),
    {ok, Funcs} = ephp_func:start_link(),
    {ok, Output} = ephp_output:start_link(Ref),
    {ok, Const} = ephp_const:start_link(),
    {ok, Inc} = ephp_include:start_link(),
    {ok, Object} = ephp_object:start_link(),
    {ok, Errors} = ephp_error:start_link(),
    {ok, Class} = ephp_class:start_link(),
    {ok, Shutdown} = ephp_shutdown:start_link(),
    {ok, _} = ephp_stack:start_link(),
    {ok, _} = ephp_mem:start_link(),
    {ok, Ref} = start_link(#state{
        ref = Ref,
        output = Output,
        funcs = Funcs,
        class = Class,
        object = Object,
        const = Const,
        include = Inc,
        shutdown = Shutdown,
        errors = Errors
    }),
    ephp_class:register_classes(Class, Ref),
    ephp_stream:start_link(),
    {ok, Ref}.

start_link(#state{ref = undefined} = State) ->
    start_link(State#state{ref = make_ref()});

start_link(#state{ref = Ref, global = Ref}) when is_reference(Ref) ->
    throw({error, ecyclerefs});

start_link(#state{ref = Ref, global = undefined} = State)
        when is_reference(Ref) ->
    {ok, Vars} = ephp_vars:start_link(),
    ephp_vars:set(Vars, #variable{name = <<"GLOBALS">>},
                  #var_ref{pid = Vars, ref = global}, Ref),
    save_state(State#state{vars = Vars}),
    {ok, Ref};

start_link(#state{ref = Ref} = State) when is_reference(Ref) ->
    {ok, Vars} = ephp_vars:start_link(),
    save_state(State#state{vars = Vars}),
    {ok, Ref}.

clone(Ref) ->
    NewRef = make_ref(),
    State = load_state(Ref),
    save_state(State#state{ref = NewRef,
                           vars = ephp_vars:clone(State#state.vars)}),
    {ok, NewRef}.

start_mirror(#state{} = State) ->
    Ref = make_ref(),
    save_state(State#state{ref = Ref}),
    {ok, Ref}.

get(Context, VarPath) ->
    #state{vars = Vars} = load_state(Context),
    ephp_vars:get(Vars, VarPath, Context).

isset(Context, VarPath) ->
    #state{vars = Vars} = load_state(Context),
    ephp_vars:isset(Vars, VarPath, Context).

empty(Context, VarPath) ->
    #state{vars = Vars} = load_state(Context),
    ephp_vars:empty(Vars, VarPath, Context).

set_bulk(_Context, []) -> ok;
set_bulk(Context, VarVals) ->
    State = load_state(Context),
    ephp_vars:set_bulk(State#state.vars, VarVals, Context),
    ok.

set(Context, VarPath, Value) ->
    State = load_state(Context),
    ephp_vars:set(State#state.vars, get_var_path(VarPath, State), Value, Context),
    ok.

del(Context, VarPath) ->
    State = load_state(Context),
    ephp_vars:del(State#state.vars, get_var_path(VarPath, State), Context),
    ok.

get_meta(Context, Key) ->
    #state{meta = Meta} = load_state(Context),
    case lists:keyfind(Key, 1, Meta) of
        false -> undefined;
        {Key, Value} -> Value
    end.

set_meta(Context, Key, Value) ->
    #state{meta = Meta} = State = load_state(Context),
    NewMeta = lists:keystore(Key, 1, Meta, {Key, Value}),
    save_state(State#state{meta = NewMeta}),
    ok.

solve(Context, Expression) ->
    State = load_state(Context),
    {Value, NewState} = resolve(Expression, State),
    save_state(NewState),
    Value.

destroy(Context) ->
    erlang:erase(Context),
    ok.

destroy_all(Context) ->
    State = load_state(Context),
    ephp_object:destroy(Context, State#state.object),
    ephp_class:destroy(State#state.class),
    ephp_output:destroy(State#state.output),
    ephp_const:destroy(State#state.const),
    ephp_include:destroy(State#state.include),
    ephp_func:destroy(State#state.funcs),
    ephp_error:destroy(State#state.errors),
    ephp_vars:destroy(Context, State#state.vars),
    ephp_shutdown:destroy(State#state.shutdown),
    ephp_stack:destroy(),
    ephp_mem:stop(),
    destroy(Context).

get_vars(Context) ->
    (load_state(Context))#state.vars.

get_consts(Context) ->
    (load_state(Context))#state.const.

get_objects(Context) ->
    (load_state(Context))#state.object.

get_classes(Context) ->
    (load_state(Context))#state.class.

get_funcs(Context) ->
    (load_state(Context))#state.funcs.

register_func(Context, PHPFunc, ModuleOrArgs, FunOrCode, VA) ->
    register_func(Context, PHPFunc, ModuleOrArgs, FunOrCode, false, VA).

register_func(Context, PHPFunc, ModuleOrArgs, FunOrCode, PackArgs, VA) ->
    register_func(Context, [], PHPFunc, ModuleOrArgs, FunOrCode, PackArgs, VA).

register_func(Context, NS, PHPFunc, Module, Fun, PackArgs, VA)
        when is_atom(Module) andalso is_atom(Fun) ->
    #state{funcs = Funcs, active_file = File} = load_state(Context),
    AbsFile = filename:absname(File),
    ephp_func:register_func(Funcs, AbsFile, NS, PHPFunc, Module, Fun, PackArgs, VA),
    ok;

register_func(Context, NS, PHPFunc, Args, Code, PackArgs, VA) ->
    #state{funcs = Funcs, active_file = File} = load_state(Context),
    AbsFile = filename:absname(File),
    ephp_func:register_func(Funcs, AbsFile, NS, PHPFunc, Args, Code, PackArgs, VA),
    ok.

get_functions(Context) ->
    #state{funcs = Funcs} = load_state(Context),
    ephp_func:get_functions(Funcs).

get_function(Context, FuncName) ->
    get_function(Context, [], FuncName).

get_function(Context, NS, FuncName) ->
    #state{funcs = Funcs} = load_state(Context),
    ephp_func:get(Funcs, NS, FuncName).

is_defined_function(Context, FuncName) ->
    #state{funcs = Funcs} = load_state(Context),
    ephp_func:is_defined(Funcs, FuncName).

get_active_function(Context) ->
    #state{active_fun = ActiveFun} = load_state(Context),
    ActiveFun.

get_active_function_ns(Context) ->
    #state{active_fun_ns = ActiveFunNS} = load_state(Context),
    ActiveFunNS.

get_active_function_arity(Context) ->
    #state{active_fun_args = ActiveFunArgs} = load_state(Context),
    ActiveFunArgs.

get_errors_id(Context) ->
    #state{errors = Errors} = load_state(Context),
    Errors.

set_errors_id(Context, Errors) ->
    State = load_state(Context),
    save_state(State#state{errors = Errors}),
    ok.

is_const_defined(Context, NS, ClassName, Name) ->
    #state{const = Const} = load_state(Context),
    ephp_const:is_defined(Const, NS, ClassName, Name).

get_const(Context, Name, Index) ->
    #state{const = Const} = load_state(Context),
    ephp_const:get(Const, Name, Index, Context).

get_const(Context, NS, Name, Index) ->
    #state{const = Const} = load_state(Context),
    ephp_const:get(Const, NS, undefined, Name, Index, Context).

get_const(Context, NS, Class, Name, Index) ->
    #state{const = Const} = load_state(Context),
    ephp_const:get(Const, NS, Class, Name, Index, Context).

register_const(Context, Name, Value) ->
    #state{const = Const} = load_state(Context),
    ephp_const:set(Const, Name, Value),
    ok.

register_const(Context, NS, Name, Value) ->
    #state{const = Const} = load_state(Context),
    ephp_const:set(Const, NS, undefined, Name, Value),
    ok.

call_function(Context, Call) ->
    {Val, NS} = resolve(Call, load_state(Context)),
    save_state(NS),
    Val.

call_method(Context, Instance, Call) ->
    {Val, NS} = run_method(Instance, Call, load_state(Context)),
    save_state(NS),
    Val.

get_active_file(Context) ->
    (load_state(Context))#state.active_file.

get_active_class(Context) ->
    (load_state(Context))#state.active_class.

get_active_class_ns(Context) ->
    (load_state(Context))#state.active_class_ns.

get_active_real_class(Context) ->
    (load_state(Context))#state.active_real_class.

get_active_real_class_ns(Context) ->
    (load_state(Context))#state.active_real_class_ns.

set_active_file(Context, undefined) ->
    Filename = <<"php shell code">>,
    State = load_state(Context),
    save_state(State#state{active_file = Filename}),
    ok;

set_active_file(Context, Filename) ->
    State = load_state(Context),
    save_state(State#state{active_file = Filename}),
    ok.

set_active_class(Context, NS, ClassName) ->
    #state{const = Const} = State = load_state(Context),
    save_state(State#state{active_class = ClassName,
                           active_class_ns = NS}),
    ephp_const:set(Const, <<"__CLASS__">>, ephp_ns:to_bin(NS, ClassName)),
    ok.

set_active_real_class(Context, NS, ClassName) ->
    State = load_state(Context),
    save_state(State#state{active_real_class = ClassName,
                           active_real_class_ns = NS}),
    ok.

get_output(Context) ->
    #state{output=Output} = load_state(Context),
    ephp_output:pop(Output).

set_output(Context, Text) ->
    #state{output=Output} = load_state(Context),
    ephp_output:push(Output, Text),
    ok.

set_output_handler(Context, Output) ->
    State = load_state(Context),
    ephp_output:destroy(State#state.output),
    save_state(State#state{output=Output}),
    ok.

get_output_handler(Context) ->
    #state{output=Output} = load_state(Context),
    Output.

load(Context, File) ->
    #state{include = Inc, active_file = CFile} = State = load_state(Context),
    set_active_file(State#state.ref, File),
    Return = ephp_include:load(Inc, File),
    set_active_file(State#state.ref, CFile),
    Return.

load_once(Context, File) ->
    #state{include = Inc, active_file = CFile} = State = load_state(Context),
    set_active_file(State#state.ref, File),
    Return = ephp_include:load_once(Inc, File),
    set_active_file(State#state.ref, CFile),
    Return.

register_class(Context, Class) ->
    #state{class = Classes,
           active_file = File,
           global = GlobalCtx} = load_state(Context),
    AbsFile = filename:absname(File),
    RealCtx = case GlobalCtx of
        undefined -> Context;
        _ -> GlobalCtx
    end,
    ephp_class:register_class(Classes, AbsFile, RealCtx, Class),
    ok.

register_interface(Context, Interface) ->
    #state{active_file = File} = load_state(Context),
    AbsFile = filename:absname(File),
    ephp_class:register_interface(Context, AbsFile, Interface),
    ok.

set_class_alias(Context, ClassNS, ClassName, ClassAliasNS, ClassAlias) ->
    #state{class = Classes} = load_state(Context),
    ephp_class:set_alias(Classes, ClassNS, ClassName, ClassAliasNS, ClassAlias).

set_global(Context, GlobalContext) ->
    State = load_state(Context),
    save_state(State#state{global = GlobalContext}),
    ok.

get_global(Context) ->
    (load_state(Context))#state.global.

generate_subcontext(Context) ->
    generate_subcontext(Context, Context).

generate_subcontext(LocalContext, GlobalContext) ->
    State = load_state(LocalContext),
    start_link(State#state{ref = undefined, global = GlobalContext}).

register_shutdown_func(Context, FuncName) ->
    register_shutdown_func(Context, [], FuncName).

register_shutdown_func(Context, NS, FuncName) ->
    #state{shutdown = Ref} = load_state(Context),
    ephp_shutdown:register_func(Ref, NS, FuncName).

unregister_shutdown_func(Context, FuncName) ->
    unregister_shutdown_func(Context, [], FuncName).

unregister_shutdown_func(Context, NS, FuncName) ->
    #state{shutdown = Ref} = load_state(Context),
    ephp_shutdown:unregister_func(Ref, NS, FuncName).

get_shutdown_funcs(Context) ->
    #state{shutdown = Ref} = load_state(Context),
    ephp_shutdown:get_funcs(Ref).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

resolve(true, State) ->
    {true, State};

resolve(false, State) ->
    {false, State};

resolve(undefined, State) ->
    {undefined, State};

%% TODO: reference from/to class var
resolve(#assign{variable = #variable{type = normal} = Var,
                expression = #ref{var = RefVar}},
        #state{ref = Ref, vars = Vars} = State) ->
    try
        VarPath = get_var_path(Var, State),
        %% TODO: review indexes like: $a = &$b->f();
        {NewIdx, NState} = resolve_idx(RefVar#variable.idx, State),
        ephp_vars:ref(Vars, VarPath,
                        Vars, RefVar#variable{idx = NewIdx},
                        Ref),
        resolve(RefVar, NState)
    catch
        throw:{error, _Reason} ->
            {undefined, State}
    end;

resolve(#assign{variable = #variable{name = <<"this">>, idx = []}} = Assign,
        _State) ->
    ephp_error:error({error, eassignthis, Assign#assign.line,
        ?E_ERROR, {}});

resolve(#assign{variable = #variable{type = normal} = Var,
                expression = Expr},
        #state{ref = Ref} = State) ->
    {Value, NState} = resolve(Expr, State),
    try
        VarPath = get_var_path(Var, NState),
        ephp_vars:set(NState#state.vars, VarPath, Value, Ref),
        case Expr of
            #instance{} -> ephp_object:remove(Ref, Value);
            #clone{} -> ephp_object:remove(Ref, Value);
            #cast{type = object} -> ephp_object:remove(Ref, Value);
            #call{} when ?IS_OBJECT(Value) -> ephp_object:remove(Ref, Value);
            #call{} when ?IS_MEM(Value) -> ephp_mem:remove(Value);
            _ -> ok
        end,
        {Value, NState}
    catch
        throw:{error, _Reason} ->
            {undefined, NState}
    end;

resolve(#assign{variable = #variable{type = class,
                                     class = <<"self">>,
                                     line = Index}},
        #state{active_class = <<>>}) ->
    ephp_error:error({error, enoclassscope, Index, ?E_ERROR, {<<"self">>}});

resolve(#assign{variable = #variable{type = class,
                                     class = <<"self">>} = Var} = Assign,
        #state{active_class = ClassName} = State) ->
    resolve(Assign#assign{variable = Var#variable{class = ClassName}}, State);

%% TODO errors for parent
resolve(#assign{variable = #variable{type = class,
                                     class = <<"parent">>} = Var} = Assign,
        #state{class = Classes,
               active_class = ClassName,
               active_class_ns = ClassNS} = State) ->
    %% TODO error in case there are no parent
    {ok, #class{extends = ParentName}} = ephp_class:get(Classes, ClassNS, ClassName),
    resolve(Assign#assign{variable = Var#variable{class = ParentName}}, State);

resolve(#assign{variable = #variable{type = class,
                                     class = ClassName,
                                     class_ns = NS,
                                     line = Index} = Var,
                expression = Expr},
        #state{ref = Ref, class = Classes} = State) ->
    try
        VarPath = get_var_path(Var, State),
        {Value, NState} = resolve(Expr, State),
        case ephp_class:get(Classes, NS, ClassName) of
            {ok, #class{static_context = ClassCtx}} ->
                Result = set(ClassCtx, VarPath, Value),
                case Expr of
                    #instance{} -> ephp_object:remove(Ref, Value);
                    #clone{} -> ephp_object:remove(Ref, Value);
                    #cast{type = object} -> ephp_object:remove(Ref, Value);
                    #call{} when ?IS_OBJECT(Value) ->
                        ephp_object:remove(Ref, Value);
                    #call{} when ?IS_MEM(Value) ->
                        ephp_mem:remove(Value);
                    _ -> ok
                end,
                Result;
            {error, enoexist} ->
                ephp_error:error({error, eundefclass, Index,
                                    ?E_ERROR, {NS, ClassName}})
        end,
        {Value, NState}
    catch
        throw:{error, _Reason} ->
            {undefined, State}
    end;

resolve(#assign{variable = #assign{} = A, expression = Expr}, State) ->
    #assign{variable = V1, expression = V2} = A,
    {Value, NState} = resolve(#assign{variable = V2, expression = Expr}, State),
    resolve(#assign{variable = V1, expression = Value}, NState);

resolve(#assign{variable = #variable{type = static, idx = []} = Var,
                expression = Expr},
        #state{active_real_class = <<>>, active_fun = <<>>, ref = Ref} = State) ->
    %% TODO check if with include the normal behaviour changes
    {Value, NState} = resolve(Expr, State),
    try
        VarPath = get_var_path(Var, NState),
        ephp_vars:set(NState#state.vars, VarPath, Value, Ref),
        case Expr of
            #instance{} -> ephp_object:remove(Ref, Value);
            #clone{} -> ephp_object:remove(Ref, Value);
            #cast{type = object} -> ephp_object:remove(Ref, Value);
            #call{} when ?IS_OBJECT(Value) -> ephp_object:remove(Ref, Value);
            #call{} when ?IS_MEM(Value) -> ephp_mem:remove(Value);
            _ -> ok
        end,
        {Value, NState}
    catch
        throw:{error, _Reason} ->
            {undefined, NState}
    end;

resolve(#assign{variable = #variable{type = static, name = VarName, idx = []},
                expression = Expr},
        #state{funcs = Funcs, ref = Ref, vars = Vars,
               active_fun = ActiveFun, active_fun_ns = ActiveFunNS,
               active_real_class = <<>>} = State) ->
    {Value, NState} = resolve(Expr, State),
    RealValue = ephp_func:init_static_value(Funcs, ActiveFunNS, ActiveFun, VarName, Value),
    ephp_vars:set(Vars, #variable{name = VarName}, RealValue, Ref),
    case Expr of
        #instance{} -> ephp_object:remove(Ref, Value);
        #clone{} -> ephp_object:remove(Ref, Value);
        #cast{type = object} -> ephp_object:remove(Ref, Value);
        #call{} when ?IS_OBJECT(Value) -> ephp_object:remove(Ref, Value);
        #call{} when ?IS_MEM(Value) -> ephp_mem:remove(Value);
        _ -> ok
    end,
    {Value, NState};

resolve(#assign{variable = #variable{type = static, name = VarName, idx = []},
                expression = Expr},
        #state{class = Classes, ref = Ref, vars = Vars,
               active_fun = ActiveFun,
               active_real_class = ActiveClass,
               active_real_class_ns = ActiveClassNS} = State) ->
    {Value, NState} = resolve(Expr, State),
    RealValue = ephp_class:init_static_value(Classes, ActiveClassNS, ActiveClass,
                                             ActiveFun, VarName, Value),
    ephp_vars:set(Vars, #variable{name = VarName}, RealValue, Ref),
    case Expr of
        #instance{} -> ephp_object:remove(Ref, Value);
        #clone{} -> ephp_object:remove(Ref, Value);
        #cast{type = object} -> ephp_object:remove(Ref, Value);
        #call{} when ?IS_OBJECT(Value) -> ephp_object:remove(Ref, Value);
        #call{} when ?IS_MEM(Value) -> ephp_mem:remove(Value);
        _ -> ok
    end,
    {Value, NState};

resolve(#assign{variable = #call{name = <<"list">>, args = Args}=List,
                expression = Expr}, State) ->
    {Value, NState} = resolve(Expr, State),
    resolve(List#call{args = [Value|Args]}, NState);

resolve(#operation{} = Op, State) ->
    resolve_op(Op, State);

resolve(#int{int = Int}, State) ->
    {Int, State};

resolve(N, State) when is_number(N) ->
    {N, State};

resolve(S, State) when is_binary(S) ->
    {S, State};

resolve(A, State) when ?IS_ARRAY(A) ->
    {A, State};

resolve(#float{float = Float}, State) ->
    {Float, State};

resolve(#text{text = Text}, State) ->
    {Text, State};

resolve(#text_to_process{text = Texts, line = Line}, State) ->
    resolve_txt(Texts, Line, State);

resolve(Object, State) when ?IS_OBJECT(Object) ->
    {Object, State};

resolve({pre_incr, Var, _Line}, #state{ref = Ref} = State) ->
    try
        VarPath = get_var_path(Var, State),
        Value = ephp_vars:get(State#state.vars, VarPath, Ref),
        NewValue = rich_increment(Value),
        if
            ?IS_MEM(Value) -> ephp_mem:set(Value, NewValue);
            true -> ephp_vars:set(State#state.vars, VarPath, NewValue, Ref)
        end,
        {NewValue, State}
    catch
        throw:{error, _Reason} ->
            {undefined, State}
    end;

resolve({pre_decr, Var, _Line}, #state{ref = Ref} = State) ->
    VarPath = get_var_path(Var, State),
    OldValue = ephp_vars:get(State#state.vars, VarPath, Ref),
    case decrement(OldValue) of
        {_, OldValue} ->
            {OldValue, State};
        {_, NewValue} when ?IS_MEM(NewValue) ->
            ephp_mem:set(OldValue, NewValue),
            {NewValue, State};
        {_, NewValue} ->
            ephp_vars:set(State#state.vars, VarPath, NewValue, Ref),
            {NewValue, State}
    end;

resolve({post_incr, Var, _Line}, #state{ref = Ref} = State) ->
    VarPath = get_var_path(Var, State),
    Value = ephp_vars:get(State#state.vars, VarPath, Ref),
    {OldValue, NewValue} = increment(Value),
    if
        ?IS_MEM(Value) -> ephp_mem:set(Value, NewValue);
        true -> ephp_vars:set(State#state.vars, VarPath, NewValue, Ref)
    end,
    {OldValue, State};

resolve({post_decr, Var, _Line}, #state{ref = Ref} = State) ->
    VarPath = get_var_path(Var, State),
    CurValue = ephp_vars:get(State#state.vars, VarPath, Ref),
    case decrement(CurValue) of
        {CurValue, CurValue} ->
            {CurValue, State};
        {OldValue, NewValue} when ?IS_MEM(NewValue) ->
            ephp_mem:set(CurValue, NewValue),
            {OldValue, State};
        {OldValue, NewValue} ->
            ephp_vars:set(State#state.vars, VarPath, NewValue, Ref),
            {OldValue, State}
    end;

resolve({operation_minus, Expr, Line}, #state{ref = Ctx} = State) ->
    case resolve(Expr, State) of
        {Number, NewState} when is_number(Number) ->
            {-Number, NewState};
        {Binary, NewState} when is_binary(Binary) ->
            {-ephp_data:bin_to_number(Binary), NewState};
        {Array, _NewState} when ?IS_ARRAY(Array) ->
            ephp_error:error({error, eunsupportop, Line, ?E_ERROR, {}});
        {#obj_ref{} = ObjRef, NewState} ->
            ClassName = ephp_object:get(ObjRef),
            File = State#state.active_file,
            Data = {ClassName, <<"int">>},
            Error = {error, enocast, Line, File, ?E_NOTICE, Data},
            ephp_error:handle_error(Ctx, Error),
            {-1, NewState}
    end;

resolve({operation_not, Expr, Line}, State) ->
    EmptyArray = ephp_array:new(),
    case resolve(Expr, State) of
        {false, NewState} -> {true, NewState};
        {<<>>, NewState} -> {true, NewState};
        {0, NewState} -> {true, NewState};
        {<<"0">>, NewState} -> {true, NewState};
        {EmptyArray, NewState} -> {true, NewState};
        {undefined, NewState} -> {true, NewState};
        {Mem, NewState} when ?IS_MEM(Mem) ->
            resolve({operation_not, ephp_mem:get(Mem), Line}, NewState);
        {_Other, NewState} ->
            {false, NewState}
    end;

resolve({operation_bnot, Expr, Line}, State) ->
    case resolve(Expr, State) of
        {Number, NewState} when is_number(Number) -> {bnot(Number), NewState};
        {Binary, NewState} when is_binary(Binary) ->
            {<< <<bnot(B)/integer>> || <<B:8/integer>> <= Binary >>, NewState};
        _ ->
            ephp_error:error({error, eunsupportop, Line, ?E_ERROR, {}})
    end;

resolve(#if_block{conditions = Cond} = IfBlock, State) ->
    case resolve_op(Cond, State) of
        {true, NewState} ->
            resolve(IfBlock#if_block.true_block, NewState);
        {false, NewState} ->
            resolve(IfBlock#if_block.false_block, NewState)
    end;

resolve(#variable{} = Var, State) ->
    resolve_var(Var, State);

resolve(#array{elements = ArrayElements}, State) ->
    {Array,NState} = lists:foldl(fun
        (#array_element{idx = auto, element = Element}, {Dict, NS}) ->
            {Value, NewState} = resolve(Element, NS),
            if
                ?IS_OBJECT(Value) ->
                    case Element of
                        #instance{} -> ok;
                        #clone{} -> ok;
                        #cast{type = object} -> ok;
                        _ -> ephp_object:add_link(Value)
                    end;
                ?IS_MEM(Value) -> ephp_mem:add_link(Value);
                true -> ok
            end,
            {ephp_array:store(auto, Value, Dict), NewState};
        (#array_element{idx = I, element = Element}, {Dict, NS}) ->
            {Value, NewState} = resolve(Element, NS),
            if
                ?IS_OBJECT(Value) ->
                    case Element of
                        #instance{} -> ok;
                        #clone{} -> ok;
                        #cast{type = object} -> ok;
                        _ -> ephp_object:add_link(Value)
                    end;
                ?IS_MEM(Value) -> ephp_mem:add_link(Value);
                true -> ok
            end,
            {Idx, ReNewState} = resolve(I, NewState),
            {ephp_array:store(Idx, Value, Dict), ReNewState}
    end, {ephp_array:new(), State}, ArrayElements),
    {Array, NState};

resolve(#concat{texts = Texts, line = Line}, State) ->
    resolve_txt(Texts, Line, State);

resolve(#call{name = #function{args = RawFuncArgs, code = Code, use = Use},
              args = RawArgs, line = Line},
        #state{ref = Ref, vars = Vars, const = Const,
               active_file = File,
               active_real_class = Class} = State) ->
    {Args, NStatePrev} = resolve_args(RawArgs, State),
    {FuncArgs, NState} = resolve_func_args(RawFuncArgs, NStatePrev),
    {ok, FuncVars} = ephp_vars:start_link(),
    {ok, SubContext} = start_mirror(NState#state{
        vars = FuncVars,
        global = Ref,
        active_fun = ?FUNC_ANON_NAME,
        active_fun_args = length(RawArgs)}),
    ephp_vars:zip_args(Vars, FuncVars, Args, FuncArgs, ?FUNC_ANON_NAME, Line, Ref),
    lists:foreach(fun
        ({#variable{} = K, V}) ->
            ephp_vars:set(FuncVars, K, V, Ref);
        ({#var_ref{pid = NVars, ref = V}, N}) ->
            ephp_vars:ref(FuncVars, N, NVars, V, Ref)
    end, Use),
    register_superglobals(Ref, FuncVars),
    ephp_const:set(Const, <<"__FUNCTION__">>, ?FUNC_ANON_NAME),
    Refs = lists:map(fun
        (#variable{} = Var) ->
            #var_ref{pid = FuncVars, ref = Var};
        (#var_ref{} = VarRef) ->
            VarRef
    end, FuncArgs),
    ephp_stack:push(File, Line, ?FUNC_ANON_NAME, Refs, Class, undefined),
    Value = case ephp_interpr:run(SubContext, #eval{statements=Code}) of
        {return, V} -> V;
        _ -> undefined
    end,
    destroy(SubContext),
    ephp_vars:destroy(Ref, FuncVars),
    ephp_const:set(Const, <<"__FUNCTION__">>, State#state.active_fun),
    ephp_stack:pop(),
    {Value, NState};

resolve(#call{name = Object} = Call, State) when ?IS_OBJECT(Object) ->
    Invoke = Call#call{name = <<"__invoke">>, type = object},
    Class = ephp_object:get_class(Object),
    case ephp_class:get_method(Class, <<"__invoke">>) of
        #class_method{} ->
            run_method(Object, Invoke, State);
        undefined ->
            ephp_error:error({error, enostrfunc, Call#call.line, ?E_ERROR, {}})
    end;

resolve(#call{name = Fun} = Call, State) when ?IS_ARRAY(Fun) ->
    case ephp_array:to_list(Fun) of
        [{_, ObjRef}, {_, Name}] when ?IS_OBJECT(ObjRef) andalso is_binary(Name) ->
            RealCall = Call#call{name = Name, type = object},
            run_method(ObjRef, RealCall, State);
        [{_, ClassName}, {_, Name}] when is_binary(ClassName) andalso is_binary(Name) ->
            {NS, CName} = ephp_ns:parse(ClassName),
            {ok, Class} = ephp_class:get(State#state.ref, NS, CName, true),
            RealCall = Call#call{name = Name, type = class, namespace = NS,
                                 class = CName},
            run_method(Class, RealCall, State);
        _Other ->
            %% FIXME: something more here?
            throw({error, implementation, Call#call.line, ?E_ERROR, {_Other}})
    end;

resolve(#call{name = Fun} = Call, State) when not is_binary(Fun) ->
    {RawName, NewState} = resolve(Fun, State),
    if
        RawName =:= Fun ->
            %% FIXME: only to avoid infinite-loop
            throw({error, implementation, Call#call.line, ?E_ERROR, {RawName}});
        is_binary(RawName) ->
            {FunNS, RealName} = ephp_ns:parse(RawName),
            %% Note that dynamic is always using absolute (even if it's not starting with '\'.
            %% so I#call.namespace is discarded.
            RealNS = ephp_ns:normalize(FunNS),
            resolve(Call#call{name = RealName, namespace = RealNS}, NewState);
        true ->
            resolve(Call#call{name = RawName}, NewState)
    end;

resolve(#call{type = normal, name = Fun, namespace = NS} = Call,
        #state{funcs = Funcs} = State) ->
    resolve_function(Call, ephp_func:get(Funcs, NS, Fun), State);

%% TODO error if no class scope
resolve(#call{type = class, class = <<"self">>} = Call,
        #state{active_class = Name} = State) ->
    resolve(Call#call{class = Name}, State);

%% TODO error if no class scope
resolve(#call{type = class, class = <<"parent">>, namespace = NS} = Call,
        #state{active_real_class = Name, class = Classes} = State) ->
    %% TODO error if no parent defined
    {ok, #class{extends = Extends}} = ephp_class:get(Classes, NS, Name),
    %% TODO check name for class (parent or grandpa, ...)
    resolve(Call#call{class = Extends}, State);

resolve(#call{type = class, class = CurrentClassName} = Call,
        #state{active_class = CurrentClassName,
               ref = Ref, vars = Vars} = State) ->
    Object = ephp_vars:get(Vars, #variable{name = <<"this">>}, Ref),
    run_method(Object, Call, State);

resolve(#call{type = class, class = Name, namespace = NS, line = Index} = Call,
        #state{class = Classes, active_class = <<>>} = State) ->
    case ephp_class:get(Classes, NS, Name) of
        {ok, #class{} = Class} ->
            %% FIXME: check when this call to ensure is working correctly.
            run_method(Class, Call, State);
        {error, enoexist} ->
            ephp_error:error({error, eundefclass, Index, ?E_ERROR, {NS, Name}})
    end;

resolve(#call{type = class, class = Name, line = Index} = Call,
        #state{class = Classes,
               active_class = CurrentClassName,
               active_class_ns = NS,
               ref = Ref} = State) ->
    {ok, #class{} = CurrentClass} = ephp_class:get(Classes, NS, CurrentClassName),
    case ephp_data:instance_of(Ref, CurrentClass, Name) of
        true ->
            Object = ephp_vars:get(State#state.vars,
                                   #variable{name = <<"this">>}, Ref),
            run_method(Object, Call, State);
        false ->
            ephp_error:error({error, eincompatctx, Index, ?E_ERROR,
                              {Name, Call#call.name}})
    end;

resolve({object, Idx, _Line} = Object, State) when is_binary(Idx)
                                            orelse is_record(Idx, call) ->
    {Object, State};

resolve({object, IdxToProcess, Line}, State) ->
    {Idx, NState} = resolve(IdxToProcess, State),
    {{object, Idx, Line}, NState};

resolve(#instance{name = ClassName} = I, State) when not is_binary(ClassName) ->
    {RawClassName, NState} = resolve(ClassName, State),
    {ClassNS, RClassName} = ephp_ns:parse(RawClassName),
    %% Note that dynamic is always using absolute (even if it's not starting with '\'.
    %% so I#instance.namespace is discarded.
    RClassNS = ephp_ns:normalize(ClassNS),
    resolve(I#instance{name = RClassName, namespace = RClassNS}, NState);

resolve(#instance{name = ClassName, namespace = ClassNS, args = RawArgs, line = Line} = Instance,
        #state{ref = LocalCtx, class = Classes, global = GlobalCtx} = State) ->
    Object = ephp_class:instance(Classes, LocalCtx, GlobalCtx, ClassNS, ClassName, Line),
    #obj_ref{pid = Objects, ref = ObjectId} = Object,
    #ephp_object{class = Class} = Obj = ephp_object:get(Object),
    ephp_object:set(Objects, ObjectId, Obj#ephp_object{instance = Instance}),
    case ephp_class:get_constructor(Classes, Class) of
        undefined ->
            {Object, State};
        #class_method{name = ConstructorName} = ClassMethod ->
            Call = #call{type = object,
                         name = ConstructorName,
                         args = RawArgs,
                         line = Line},
            #class_method{name = MethodName,
                          class_name = MCName,
                          access = Access} = ClassMethod,
            IsChild = ephp_data:instance_of(LocalCtx, Class, MCName),
            ActiveClass = State#state.active_class,
            maybe_ecallprivate_log(ActiveClass, MCName, Access, IsChild, MethodName, Line),
            {_, NState} = run_method(Object, Call, State),
            {Object, NState}
    end;

resolve({global, _Var, _Line}, #state{global = undefined} = State) ->
    {undefined, State};

resolve({global, GVars, _Line},
        #state{ref = Ref, global = GlobalCtx, vars = Vars} = State) ->
    #state{vars = GlobalVars} = load_state(GlobalCtx),
    lists:foreach(fun(GlobalVar) ->
        ephp_vars:ref(Vars, GlobalVar, GlobalVars, GlobalVar, Ref)
    end, GVars),
    {undefined, State};

resolve(#constant{type = class, class = <<"self">>, line = Index},
        #state{active_class = <<>>}) ->
    ephp_error:error({error, enoclassscope, Index, ?E_ERROR, {<<"self">>}});

resolve(#constant{type = class, class = <<"self">>, name = Name, line = Index,
                  namespace = NameSpace},
        #state{ref = Ref, const = Const,
               active_class = ClassName} = State) ->
    {ephp_const:get(Const, NameSpace, ClassName, Name, Index, Ref), State};

%% TODO error if there are no active class
resolve(#constant{type = class, class = <<"parent">>, name = Name,
                  line = Index},
        #state{ref = Ref, const = Const, class = Classes,
               active_real_class = ClassName,
               active_real_class_ns = ClassNS} = State) ->
    %% TODO: error if the parent isn't defined
    {ok, #class{namespace = NameSpace, extends = ParentName}} =
        ephp_class:get(Classes, ClassNS, ClassName),
    %% TODO check if there are a parent of a parent...
    {ephp_const:get(Const, NameSpace, ParentName, Name, Index, Ref), State};

resolve(#constant{type = class, class = #variable{} = Var, name = Name,
                  line = Line},
        #state{ref = Ref, const = Const} = State) ->
    {ObjRef, NState} = resolve(Var, State),
    #ephp_object{class = #class{namespace = NameSpace, name = ClassName}} =
        ephp_object:get(ObjRef),
    {ephp_const:get(Const, NameSpace, ClassName, Name, Line, Ref), NState};

resolve(#constant{type = class, class = ClassName, name = Name, line =  Line,
                  namespace = NameSpace},
        #state{ref = Ref, const = Const} = State) ->
    {ephp_const:get(Const, NameSpace, ClassName, Name, Line, Ref), State};

resolve(#constant{type = normal, name = Name, line = Line,
                  namespace = NameSpace},
        #state{ref = Ref, const = Const} = State) ->
    {ephp_const:get(Const, NameSpace, undefined, Name, Line, Ref), State};

resolve(#print_text{text = Text}, #state{output = Output} = State) ->
    ephp_output:push(Output, Text),
    {1, State};

resolve(undefined, State) ->
    {undefined, State};

resolve(#ref{var = #variable{} = Var}, #state{ref = Ctx, vars = Vars} = State) ->
    Ref = case ephp_vars:get(Vars, Var, Ctx) of
        ObjRef when ?IS_OBJECT(ObjRef) ->
            ObjRef;
        MemRef when ?IS_MEM(MemRef) ->
            MemRef;
        Other ->
            MemRef = ephp_mem:add(Other),
            ephp_vars:set(Vars, Var, MemRef, Ctx),
            MemRef
    end,
    {Ref, State};

resolve(auto, _State) ->
    ephp_error:error({error, earrayundef, undefined, ?E_ERROR, {<<>>}});

resolve({silent, Statement}, #state{errors=Errors}=State) ->
    ephp_error:run_quiet(Errors, fun() ->
        resolve(Statement, State)
    end);

resolve(#function{name = undefined, use = Use, line = Line} = Anon,
        #state{vars = Vars} = State) ->
    {NewUse, NState} = lists:foldl(fun
        (#variable{} = K, {Acc, S}) ->
            {V, NewState} = resolve(K, S),
            {Acc ++ [{K, V}], NewState};
        (#ref{var = #variable{} = V}, {Acc, S}) ->
            {Acc ++ [{#var_ref{pid = Vars, ref = V}, V}], S}
    end, {[], State}, Use),
    ClassName = <<"Closure">>,
    Instance = #instance{name = ClassName, line = Line},
    {Object, RState} = resolve(Instance, NState),
    case Anon#function.args of
        [] -> ok;
        Args ->
            Params = resolve_params_anon(Args),
            ephp_object:set_attr(Object, #variable{name = <<"parameter">>}, Params)
    end,
    Ctx = ephp_object:get_context(Object),
    ephp_context:set_meta(Ctx, invoke, Anon#function{use = NewUse}),
    {Object, RState};

resolve(#cast{type = Type, content = C, line = Line}, State) ->
    {Value, NState} = resolve(C, State),
    {resolve_cast(State, Line, Type, Value), NState};

resolve(#clone{var = Var, line = Line}, State) ->
    case resolve(Var, State) of
        {ObjRef, NState} when ?IS_OBJECT(ObjRef) ->
            {ephp_object:clone(State#state.ref, ObjRef), NState};
        _ ->
            ephp_error:error({error, enoclone, Line, ?E_ERROR, {}})
    end;

resolve(Unknown, _State) ->
    ephp_error:error({error, eundeftoken, undefined, ?E_CORE_ERROR, Unknown}).


decrement(undefined) -> {undefined, undefined};
decrement(Value) when is_number(Value) -> {Value, Value - 1};
decrement(Value) when ?IS_MEM(Value) -> decrement(ephp_mem:get(Value));
decrement(Value) -> {Value, Value}.

increment(undefined) -> {undefined, undefined};
increment(Value) when is_number(Value) -> {Value, Value + 1};
increment(Value) when is_binary(Value) andalso byte_size(Value) > 0 ->
    try
        Val = binary_to_integer(Value),
        {Val, Val + 1}
    catch error:badarg ->
        {Value, ephp_data:increment_code(Value)}
    end;
increment(Value) when ?IS_MEM(Value) ->
    increment(ephp_mem:get(Value));
increment(Value) -> {Value, Value}.

rich_increment(undefined) -> 1;
rich_increment(Value) when is_number(Value) -> Value + 1;
rich_increment(Value) when is_binary(Value) andalso byte_size(Value) > 0 ->
    try
        binary_to_integer(Value) + 1
    catch error:badarg ->
        ephp_data:increment_code(Value)
    end;
rich_increment(Value) when ?IS_MEM(Value) ->
    rich_increment(ephp_mem:get(Value));
rich_increment(Value) -> Value.

resolve_function(#call{name = Fun, args = RawArgs, line = Index,
                       namespace = NS},
                 {ok, #reg_func{type = builtin, pack_args = PackArgs,
                                builtin = {M, F},
                                validation_args = no_resolve}},
                 #state{ref = Ref} = State) ->
    FState = State#state{active_fun = Fun,
                         active_fun_ns = NS},
    {Args, NState} = resolve_args(no_resolve, RawArgs, FState, Index),
    save_state(NState),
    Value = if
        PackArgs -> erlang:apply(M, F, [Ref, Index, Args]);
        true -> erlang:apply(M, F, [Ref, Index|Args])
    end,
    destroy_args(NState, Args),
    {Value, (load_state(Ref))#state{ref = Ref}};

resolve_function(#call{name = Fun, args = RawArgs, line = Index,
                       namespace = NS} = _Call,
                 {ok, #reg_func{type = builtin, pack_args = PackArgs,
                                builtin = {M, F},
                                validation_args = VA}},
                 #state{ref = Ref, active_file = File} = State) ->
    FState = State#state{active_fun = Fun,
                         active_fun_ns = NS},
    VArgs = case VA of
        undefined ->
            undefined;
        {_Min, _Max, _RetErr, _ValArgs} ->
            VA;
        _ when is_list(VA) ->
            {expected_min_args(VA), expected_max_args(M, F), undefined, VA}
    end,
    try resolve_args(VArgs, RawArgs, FState, Index) of
        {Args, NState} ->
            ResArgs = [ Val || {_Var, Val} <- Args ],
            ephp_stack:push(File, Index, Fun, ResArgs,
                            undefined, undefined),
            save_state(NState),
            Value = if
                PackArgs -> erlang:apply(M,F,[Ref,Index,Args]);
                true -> erlang:apply(M,F,[Ref,Index|Args])
            end,
            ephp_stack:pop(),
            destroy_args(NState, Args),
            {Value, (load_state(Ref))#state{ref = Ref}}
    catch
        throw:{return, Value} ->
            {Value, State}
    end;

resolve_function(#call{name = Fun, args = RawArgs, line = Index} = Call,
                 {ok,#reg_func{type = php, args = RawFuncArgs, file = AFile,
                               code = Code}},
                 #state{ref = Ref, vars = Vars, const = Const,
                        active_file = File, funcs = Funcs} = State) ->
    GlobalRef = case State#state.global of
        undefined -> Ref;
        GR -> GR
    end,
    {Args, NStatePrev} = resolve_args(RawArgs, State),
    {FuncArgs, NState} = resolve_func_args(RawFuncArgs, NStatePrev),
    save_state(NState),
    {ok, NewVars} = ephp_vars:start_link(),
    SArgs = length(Args),
    {ok, SubContext} = start_mirror(NState#state{vars = NewVars,
                                                 global = GlobalRef,
                                                 active_file = AFile,
                                                 active_fun = Fun,
                                                 active_class = <<>>,
                                                 active_real_class = <<>>,
                                                 active_fun_args = SArgs}),
    ephp_vars:zip_args(Vars, NewVars, Args, FuncArgs, Fun, Index, Ref),
    register_superglobals(GlobalRef, NewVars),
    FullFunName = ephp_ns:to_bin(Call#call.namespace, Fun),
    ephp_const:set(Const, <<"__FUNCTION__">>, FullFunName),
    Refs = lists:map(fun(#variable{} = Var) ->
                            #var_ref{pid = NewVars, ref = Var};
                        (#ref{var = Var}) ->
                            #var_ref{pid = NewVars, ref = Var};
                        (#var_ref{} = VarRef) ->
                            VarRef
                      end, FuncArgs),
    ephp_stack:push(File, Index, Fun, Refs, undefined, undefined),
    Value = case ephp_interpr:run(SubContext, #eval{statements = Code}) of
        {return, V} -> V;
        _ -> undefined
    end,
    ephp_func:set_static(Funcs, Call#call.namespace, Fun, NewVars, Ref),
    destroy(SubContext),
    ephp_vars:destroy(Ref, NewVars),
    OldFullFunName = ephp_ns:to_bin(State#state.active_fun_ns, State#state.active_fun),
    ephp_const:set(Const, <<"__FUNCTION__">>, OldFullFunName),
    ephp_stack:pop(),
    {Value, NState};

resolve_function(#call{name = Fun, line = Index, namespace = []}, error, _State) ->
    ephp_error:error({error, eundefun, Index, ?E_ERROR, {[], Fun}});

resolve_function(#call{name = Fun, line = Index, namespace = NS} = Call, error, State) ->
    case ephp_func:get(State#state.funcs, [], Fun) of
        error -> ephp_error:error({error, eundefun, Index, ?E_ERROR, {NS, Fun}});
        Else -> resolve_function(Call#call{namespace = []}, Else, State)
    end.


resolve_params_anon(Args) ->
    resolve_params_anon(Args, ephp_array:new()).

resolve_params_anon([], Array) -> Array;
resolve_params_anon([#variable{name = Name, default_value = undefined}|Args], Array) ->
    resolve_params_anon(Args, ephp_array:store(<<"$", Name/binary>>, <<"<required>">>, Array));
resolve_params_anon([#variable{name = Name}|Args], Array) ->
    resolve_params_anon(Args, ephp_array:store(<<"$", Name/binary>>, <<"<optional>">>, Array)).

register_superglobals(GlobalCtx, Vars) ->
    #state{vars = GlobalVars} = load_state(GlobalCtx),
    SuperGlobals = [
        <<"_SERVER">>,
        <<"_GET">>,
        <<"_POST">>,
        <<"_FILES">>,
        <<"_COOKIE">>,
        <<"_SESSION">>,
        <<"_REQUEST">>,
        <<"_ENV">>
    ],
    ephp_vars:ref(Vars, #variable{name = <<"GLOBALS">>}, GlobalVars,
                  global, GlobalCtx),
    lists:foreach(fun(GlobalName) ->
        GlobalVar = #variable{name = GlobalName},
        ephp_vars:ref(Vars, GlobalVar, GlobalVars, GlobalVar, GlobalCtx)
    end, SuperGlobals).

resolve_func_args(RawFuncArgs, State) ->
    lists:foldl(fun
        (#variable{default_value = Val} = Var, {Vars, S}) when Val =/= undefined ->
            {Value, NewState} = resolve(Val,S),
            {Vars ++ [Var#variable{default_value = Value}], NewState};
        (Var, {Vars, NewState}) ->
            {Vars ++ [Var], NewState}
    end, {[], State}, RawFuncArgs).

resolve_idx(RawIdx, State) ->
    lists:foldl(fun(I, {Indexes, S}) ->
        case resolve(I, S) of
            {M, NewState} when ?IS_MEM(M) -> A = ephp_mem:get(M);
            {A, NewState} -> ok
        end,
        {Indexes ++ [A], NewState}
    end, {[], State}, RawIdx).

resolve_args(undefined, State) ->
    {[], State};
resolve_args(RawArgs, State) ->
    lists:foldl(fun(Arg, {Args, S}) ->
        case resolve(Arg, S) of
            {M, NewState} when ?IS_MEM(M) -> A = ephp_mem:get(M);
            {A, NewState} -> ok
        end,
        {Args ++ [{Arg, A}], NewState}
    end, {[], State}, RawArgs).

expected_min_args(VArgs) ->
    lists:foldl(fun({_,_}, I) -> I;
                   ({_,_,_}, I) -> I;
                   (_, I) -> I+1
                end, 0, VArgs).

expected_max_args(Module, Function) ->
    lists:foldl(fun({F,I}, Max) when F =:= Function andalso I > Max -> I;
                   (_, Max) -> Max
                end, 0, Module:module_info(exports)).

resolve_args(no_resolve, RawArgs, State, _Line) ->
    Args = [ {RawArg, undefined} || RawArg <- RawArgs ],
    {Args, State};
resolve_args(_, undefined, State, _Line) ->
    {[], State};
resolve_args(undefined, RawArgs, State, _Line) ->
    resolve_args(RawArgs, State);
resolve_args({_MinArgs, MaxArgs, ReturnError, VArgs} = Validation, RawArgs, State, Line) ->
    {RestRawArgs, _I, Args, NewState} = lists:foldl(fun(Type, Acc) ->
        validate_arg({Validation, Line}, Type, Acc)
    end, {RawArgs, 1, [], State}, VArgs),
    case RestRawArgs of
        [] ->
            {Args, NewState};
        _ ->
            File = NewState#state.active_file,
            Function = NewState#state.active_fun,
            Data = {Function, MaxArgs, length(Args)+length(RestRawArgs)},
            Error = {error, ewrongmaxarity, Line, File, ?E_WARNING, Data},
            ephp_error:handle_error(NewState#state.ref, Error),
            throw({return, ReturnError})
    end.


validate_arg(_Validation, {_, Default}, {[], I, Args, S}) ->
    {[], I+1, Args ++ [{undefined, Default}], S};

validate_arg({{_, _, ReturnError, _}, Line},
             {_, undefined, Error}, {[], I, _Args, S}) ->
    throw_warning(S, Line, I, Error, undefined, ReturnError);

validate_arg(_Validation, {_, Default, _Err}, {[], I, Args, S}) ->
    {[], I+1, Args ++ [{undefined, Default}], S};

validate_arg({{MinArgs, _MaxArgs, ReturnError, _VArgs}, Line},
             _Type, {[], I, _Args, S}) ->
    File = S#state.active_file,
    Function = S#state.active_fun,
    Data = {Function, MinArgs, I-1},
    ephp_error:handle_error(S#state.ref, {error, ewrongminarity, Line,
        File, ?E_WARNING, Data}),
    throw({return, ReturnError});

validate_arg(_Validation, {raw, Default}, {[RArg|RArgs], I, Args, S}) ->
    {RArgs, I+1, Args ++ [{RArg, Default}], S};

validate_arg(_Validation, raw, {[#variable{} = RArg|RArgs], I, Args, S}) ->
    {RRArg, NewState} = resolve_indexes(RArg, S),
    {RArgs, I+1, Args ++ [{RArg, RRArg}], NewState};

validate_arg(_Validation, raw, {[RArg|RArgs], I, Args, S}) ->
    {RRArg, NewState} = resolve(RArg, S),
    {RArgs, I+1, Args ++ [{RArg, RRArg}], NewState};

validate_arg({{_MinArgs, _MaxArgs, ReturnError, _VArgs}, Line},
             VArg, {[RArg|RArgs], I, Args, S}) ->
    {A, NewState} = case resolve(RArg,S) of
        {MemRef, NS} when ?IS_MEM(MemRef) ->
            resolve(ephp_mem:get(MemRef), NS);
        {A0, NS} ->
            {A0, NS}
    end,
    check_arg(S, Line, I, VArg, A, ReturnError),
    {RArgs, I+1, Args ++ [{RArg,A}], NewState}.


check_arg(_State, _Line, _I, mixed, _A, _ReturnError) ->
    ok;
check_arg(State, Line, I, Type, MemRef, ReturnError) when ?IS_MEM(MemRef) ->
    check_arg(State, Line, I, Type, ephp_mem:get(MemRef), ReturnError);
check_arg(State, Line, I, string, A, ReturnError)
        when not is_binary(A) andalso not is_number(A) ->
    throw_warning(State, Line, I, <<"string">>, A, ReturnError);
check_arg(State, Line, I, {string, _}, A, ReturnError)
        when not is_binary(A) andalso not is_number(A) ->
    throw_warning(State, Line, I, <<"string">>, A, ReturnError);
check_arg(State, Line, I, integer, A, ReturnError)
        when not is_number(A) ->
    throw_warning(State, Line, I, <<"long">>, A, ReturnError);
check_arg(State, Line, I, {integer, _}, A, ReturnError)
        when not is_number(A) ->
    throw_warning(State, Line, I, <<"long">>, A, ReturnError);
check_arg(State, Line, I, str_or_int, A, ReturnError)
        when not is_binary(A) andalso not is_integer(A) andalso A =/= undefined ->
    throw_warning(State, Line, I, <<"long">>, A, ReturnError);
check_arg(State, Line, I, {str_or_int, _D, {Error, ErrArgs}}, A, ReturnError)
        when not is_binary(A) andalso not is_integer(A) andalso A =/= undefined
        andalso is_atom(Error) ->
    throw_warning(State, Line, I, {Error, ErrArgs}, A, ReturnError);
check_arg(State, Line, I, {str_or_int, D}, A, ReturnError)
        when not is_binary(A) andalso not is_integer(A) andalso A =/= undefined
        andalso not is_tuple(D) ->
    throw_warning(State, Line, I, <<"long">>, A, ReturnError);
check_arg(State, Line, I, double, A, ReturnError) when not is_number(A) ->
    throw_warning(State, Line, I, <<"double">>, A, ReturnError);
check_arg(State, Line, I, {double, _}, A, ReturnError) when not is_number(A) ->
    throw_warning(State, Line, I, <<"double">>, A, ReturnError);
check_arg(State, Line, I, array, A, ReturnError) when not ?IS_ARRAY(A) ->
    throw_warning(State, Line, I, <<"array">>, A, ReturnError);
check_arg(State, Line, I, {array, _}, A, ReturnError) when not ?IS_ARRAY(A) ->
    throw_warning(State, Line, I, <<"array">>, A, ReturnError);
check_arg(State, Line, I, object, A, ReturnError) when not ?IS_OBJECT(A) ->
    throw_warning(State, Line, I, <<"object">>, A, ReturnError);
check_arg(State, Line, I, {object, _}, A, ReturnError) when not ?IS_OBJECT(A) ->
    throw_warning(State, Line, I, <<"object">>, A, ReturnError);
check_arg(State, Line, I, boolean, A, ReturnError)
        when not is_boolean(A) andalso not is_number(A)
        andalso not is_binary(A) ->
    throw_warning(State, Line, I, <<"boolean">>, A, ReturnError);
check_arg(State, Line, I, {boolean, _}, A, ReturnError)
        when not is_boolean(A) andalso not is_number(A)
        andalso not is_binary(A) ->
    throw_warning(State, Line, I, <<"boolean">>, A, ReturnError);
check_arg(State, Line, I, resource, A, ReturnError) when not ?IS_RESOURCE(A) ->
    throw_warning(State, Line, I, <<"resource">>, A, ReturnError);
check_arg(State, Line, I, {resource, _}, A, ReturnError)
        when not ?IS_RESOURCE(A) ->
    throw_warning(State, Line, I, <<"resource">>, A, ReturnError);
%% TODO: check if "a valid path" could be throw with different contents in the
%%       string
check_arg(State, Line, I, path, A, ReturnError) when not is_binary(A) ->
    throw_warning(State, Line, I, <<"a valid path">>, A, ReturnError);
check_arg(State, Line, I, {path, _}, A, ReturnError) when not is_binary(A) ->
    throw_warning(State, Line, I, <<"a valid path">>, A, ReturnError);
check_arg(State, Line, I, callable, A, ReturnError) when not ?IS_CALLABLE(A) ->
    throw_warning(State, Line, I, <<"a valid callback">>, A, ReturnError);
%% TODO add more checks here!
check_arg(_State, _Line, _I, _Check, _Var, _ReturnError) ->
    ok.

throw_warning(State, Line, _I, {ErrId, ErrArgs}, _Var, ErrorRet) ->
    File = State#state.active_file,
    Function = State#state.active_fun,
    Data = {Function, ErrArgs},
    Error = {error, ErrId, Line, File, ?E_WARNING, Data},
    ephp_error:handle_error(State#state.ref, Error),
    throw({return, ErrorRet});
throw_warning(State, Line, I, Type, Var, ErrorRet) ->
    File = State#state.active_file,
    Function = State#state.active_fun,
    Data = {Function, I, Type, ephp_data:gettype(Var)},
    Error = {error, ewrongarg, Line, File, ?E_WARNING, Data},
    ephp_error:handle_error(State#state.ref, Error),
    throw({return, ErrorRet}).

zip_args(ValArgs, FuncArgs) ->
    {Result, _} = lists:foldl(fun
        (FuncArg, {Res, [{_,ArgVal}|RestArgs]}) ->
            {Res ++ [{FuncArg, ArgVal}], RestArgs};
        (#variable{default_value=Val}=FuncArg, {Res, []}) ->
            {Res ++ [{FuncArg, Val}], []};
        (_FuncArg, {Res, []}) ->
            {Res, []}
    end, {[], ValArgs}, FuncArgs),
    Result.


get_class_from(MethodVars, RegInstance, NS, AName, Line, State) when ?IS_OBJECT(RegInstance) ->
    #ephp_object{class = Class} = ephp_object:get(RegInstance),
    ephp_vars:set(MethodVars,
                  #variable{name = <<"this">>, type = object,
                            class = Class#class.name, line = Line},
                  RegInstance, State#state.ref),
    if
        AName =/= undefined andalso AName =/= Class#class.name ->
            {ok, ModifiedClass} = ephp_class:get(State#state.class, NS, AName),
            {RegInstance, ModifiedClass};
        true ->
            {RegInstance, Class}
    end;

get_class_from(_MethodVars, #class{name = AName} = Class, _NS, AName, _Line, _State) ->
    {undefined, Class};

get_class_from(_MethodVars, #class{name = undefined} = Class, _NS, _AName, _Line, _State) ->
    {undefined, Class};

get_class_from(_MethodVars, #class{}, NS, AName, _Line, State) ->
    %% TODO maybe this should require $this???
    {ok, ModifiedClass} = ephp_class:get(State#state.class, NS, AName),
    {undefined, ModifiedClass}.


maybe_enostatic_log(Class, undefined, MethodName,
                    #class_method{type = Type},
                    Line, State) when Type =/= static ->
    ephp_error:handle_error(State#state.ref, {error, enostatic, Line,
                                              State#state.active_file,
                                              ?E_STRICT,
                                              {Class#class.name,
                                               MethodName}});
maybe_enostatic_log(_Class, _Object, _MethodName, _ClassMethod,
                    _Line, _State) ->
    ok.

maybe_ecallprivate_log(ClassName, MCName, Access, IsChild, MethodName, Line)
    when (Access =:= private andalso MCName =/= ClassName) orelse
         (Access =:= protected andalso not IsChild) ->
    ephp_error:error({error, ecallprivate, Line, ?E_ERROR,
                      {ClassName, MCName, MethodName, Access}});
maybe_ecallprivate_log(_ClassName, _MCName, _Access, _IsChild, _MethodName, _Line) ->
    ok.


maybe_destroy_vars_from_method(<<"__destruct">>, _MethodVars, _Ref) -> ok;
maybe_destroy_vars_from_method(_MethodName, MethodVars, Ref) ->
    ephp_vars:destroy(Ref, MethodVars).


run_method(RegInstance, #call{name = Name} = Call, State)
        when not is_binary(Name) ->
    {<<RealName/binary>>, NewState} = resolve(Name, State),
    run_method(RegInstance, Call#call{name = RealName}, NewState);

run_method(RegInstance, #call{args = RawArgs, line = Line, class = AName} = Call,
           #state{ref = Ref, class = Classes} = State) ->
    {Args, NStatePrev} = resolve_args(RawArgs, State),
    {ok, MethodVars} = ephp_vars:start_link(),
    NS = Call#call.namespace,
    {Object, Class} = get_class_from(MethodVars, RegInstance, NS, AName, Line, State),
    #class{name = ClassName} = Class,
    ClassMethod = case Call#call.name of
        <<"__construct">> ->
            ephp_class:get_constructor(Classes, Class);
        _ ->
            ephp_class:get_method(Class, Line, Call#call.name)
    end,
    #class_method{args = RawMethodArgs,
                  name = MethodName,
                  class_name = MCName,
                  access = Access} = ClassMethod,
    IsChild = ephp_data:instance_of(Ref, RegInstance, MCName),
    maybe_ecallprivate_log(ClassName, MCName, Access, IsChild, MethodName, Line),
    {MethodArgs, NState} = resolve_func_args(RawMethodArgs, NStatePrev),
    maybe_enostatic_log(Class, Object, MethodName, ClassMethod, Line, NState),
    run_method(ClassMethod, Class, Object, Call, Args,
               MethodVars, MethodName, MethodArgs, RegInstance, NState).

run_method(#class_method{code_type = php} = ClassMethod, Class, Object,
           Call, Args, MethodVars, MethodName, MethodArgs, _RegInstance,
           #state{ref = Ref, class = Classes, vars = Vars,
                  const = Const} = State) ->
    RealObject = case Call#call.type of
        class -> undefined;
        object -> Object
    end,
    ephp_vars:zip_args(Vars, MethodVars, Args, MethodArgs, MethodName,
                        Call#call.line, Ref),
    {ok, SubContext} = start_mirror(State#state{
        vars = MethodVars,
        global = Ref,
        active_file = Class#class.file,
        active_fun = MethodName,
        active_fun_args = length(Args),
        active_real_class = Class#class.name,
        active_class = ClassMethod#class_method.class_name,
        active_class_ns = ClassMethod#class_method.namespace}),
    register_superglobals(Ref, MethodVars),
    OldMethodName = get_const(Ref, <<"__METHOD__">>, Call#call.line),
    FullClassName = ephp_ns:to_bin(Class#class.namespace,
                                   ClassMethod#class_method.class_name),
    ephp_const:set_bulk(Const, [
        {<<"__FUNCTION__">>, MethodName},
        {<<"__METHOD__">>,
            <<FullClassName/binary, "::", MethodName/binary>>}
    ]),
    %% TODO: with static (late binding) this changes
    set_active_class(Ref, ClassMethod#class_method.namespace,
                     ClassMethod#class_method.class_name),
    set_active_real_class(Ref, Class#class.namespace, Class#class.name),
    Refs = lists:map(fun(#variable{} = Var) ->
                            #var_ref{pid = MethodVars, ref = Var};
                        (#var_ref{} = VarRef) ->
                            VarRef;
                        (#ref{} = VarRef) ->
                            VarRef
                     end, MethodArgs),
    ephp_stack:push(State#state.active_file, Call#call.line,
                    MethodName, Refs, Class#class.name, RealObject),
    Code = ClassMethod#class_method.code,
    Value = case ephp_interpr:run(SubContext, #eval{statements = Code}) of
        {return, V} -> V;
        _ -> undefined
    end,
    ephp_class:set_static(Classes, Class#class.namespace, Class#class.name,
                          MethodName, MethodVars, Ref),
    destroy(SubContext),
    maybe_destroy_vars_from_method(MethodName, MethodVars, Ref),
    ephp_const:set_bulk(Const, [
        {<<"__FUNCTION__">>, State#state.active_fun},
        {<<"__METHOD__">>, OldMethodName}
    ]),
    set_active_class(Ref, State#state.active_class_ns, State#state.active_class),
    set_active_real_class(Ref, State#state.active_real_class_ns, State#state.active_real_class),
    ephp_stack:pop(),
    {Value, State};

run_method(#class_method{code_type = builtin} = ClassMethod, _Class, _Object,
           #call{args = RawArgs, line = Index},
           _Args, _MethodVars, _MethodName, MethodArgs, RegInstance,
           #state{ref = Ref} = State) ->
    {M, F} = ClassMethod#class_method.builtin,
    VArgs = case ClassMethod#class_method.validation_args of
        no_resolve ->
            no_resolve;
        undefined ->
            undefined;
        {_Min, _Max, _RetErr, _ValArgs} = VA ->
            VA;
        VA when is_list(VA) ->
            {expected_min_args(VA), expected_max_args(M, F),
                undefined, VA}
    end,
    try resolve_args(VArgs, RawArgs, State, Index) of
        {FArgs, FState} when ClassMethod#class_method.validation_args =:= no_resolve ->
            save_state(FState),
            Value = erlang:apply(M, F, [Ref, RegInstance, Index, FArgs]),
            {Value, (load_state(Ref))#state{ref = Ref}};
        {FArgs, FState} ->
            FMArgs = zip_args(FArgs, MethodArgs),
            save_state(FState),
            Value = if
                ClassMethod#class_method.pack_args ->
                    erlang:apply(M, F, [Ref, RegInstance, Index, FMArgs]);
                true ->
                    erlang:apply(M, F, [Ref, RegInstance, Index|FMArgs])
            end,
            destroy_args(State, FMArgs),
            {Value, (load_state(Ref))#state{ref=Ref}}
    catch
        throw:{return,Value} ->
            {Value, State}
    end.

destroy_args(_State, []) ->
    ok;
destroy_args(State, [{#instance{}, ObjRef}|Rest]) when ?IS_OBJECT(ObjRef) ->
    ephp_vars:destroy_data(State#state.ref, ObjRef),
    destroy_args(State, Rest);
destroy_args(State, [{#cast{}, ObjRef}|Rest]) when ?IS_OBJECT(ObjRef) ->
    ephp_vars:destroy_data(State#state.ref, ObjRef),
    destroy_args(State, Rest);
destroy_args(State, [{#array{}, Array}|Rest]) when ?IS_ARRAY(Array) ->
    ephp_vars:destroy_data(State#state.ref, Array),
    destroy_args(State, Rest);
destroy_args(State, [{#cast{}, Array}|Rest]) when ?IS_ARRAY(Array) ->
    ephp_vars:destroy_data(State#state.ref, Array),
    destroy_args(State, Rest);
destroy_args(State, [{_, _}|Rest] = _Ignore) ->
    destroy_args(State, Rest).

resolve_var(#variable{type = normal, idx = []} = Var, State) ->
    {ephp_vars:get(State#state.vars, Var, State#state.ref), State};

%% TODO check if it's not possible to run outside of a method
resolve_var(#variable{name = <<"this">>,
                      idx = [{object, #call{} = Call, _}]} = Var,
            State) ->
    InstanceVar = Var#variable{idx = [], type = object},
    Instance = ephp_vars:get(State#state.vars, InstanceVar, State#state.ref),
    run_method(Instance, Call#call{type = object}, State);

resolve_var(#variable{idx = [{object, #call{} = Call, _}]} = Var,
            #state{ref = Ref, vars = Vars} = State) ->
    InstanceVar = Var#variable{idx = []},
    Instance = case ephp_vars:get(Vars, InstanceVar, Ref) of
        ObjRef when ?IS_OBJECT(ObjRef) -> ObjRef;
        MemRef when ?IS_MEM(MemRef) -> ephp_mem:get(MemRef)
    end,
    #ephp_object{class = Class} = ephp_object:get(Instance),
    case ephp_class:get_method(Class, Call#call.line, Call#call.name) of
        #class_method{access = public} ->
            run_method(Instance, Call#call{type = object}, State);
        #class_method{access = protected} ->
            Data = {Class#class.name, Call#call.name, <<"protected">>},
            ephp_error:error({error, ecallprivate, Var#variable.line,
                              ?E_ERROR, Data});
        #class_method{access = private} ->
            Data = {Class#class.name, Call#call.name, <<"private">>},
            ephp_error:error({error, ecallprivate, Var#variable.line,
                              ?E_ERROR, Data})
    end;

resolve_var(#variable{idx = [{object, #variable{} = SubVar, Line}|Idx]} = Var,
            #state{ref = Ref, vars = Vars, active_class = RunningClass} = State) ->
    InstanceVar = Var#variable{idx = []},
    #ephp_object{class = #class{name = ClassName} = Class,
                 context = Context} =
        ephp_object:get(ephp_vars:get(Vars, InstanceVar, Ref)),
    {SubVal, State2} = resolve(SubVar, State),
    {NewVar, State3} = resolve_indexes(#variable{name = ephp_data:to_bin(SubVal),
                                                 type = object,
                                                 class = ClassName,
                                                 line = Line,
                                                 idx = Idx}, State2),
    ViaThis = (Var#variable.name =:= <<"this">>),
    case ephp_class:get_attribute(Class, SubVal) of
        #class_attr{access = protected} when not ViaThis ->
            Data = {Class#class.name, SubVal, <<"protected">>},
            ephp_error:error({error, eprivateaccess, SubVar#variable.line,
                              ?E_ERROR, Data});
        #class_attr{access = private} when not ViaThis ->
            Data = {Class#class.name, SubVal, <<"private">>},
            ephp_error:error({error, eprivateaccess, SubVar#variable.line,
                              ?E_ERROR, Data});
        #class_attr{access = private, class_name = CName, namespace = NS}
                when CName =:= RunningClass ->
            NewName = {private, NewVar#variable.name, NS, RunningClass},
            {ephp_context:get(Context, NewVar#variable{class = Class#class.name,
                                                       name = NewName,
                                                       type = object}),
                              State3};
        _ ->
            {ephp_context:get(Context, NewVar#variable{class = Class#class.name,
                                                       type = object}),
                              State3}
    end;

%% TODO implement when it's not related to "this"
resolve_var(#variable{name = <<"this">>, idx = [{object, VarName, Line}|_]},
            #state{active_class = <<>>}) when is_binary(VarName) ->
    ephp_error:error({error, enoobjthis, Line, ?E_ERROR, {}});
resolve_var(#variable{name = <<"this">>, idx = [{object, VarName, Line}|Idx]} = Var,
            #state{ref = Ref, vars = Vars,
                   active_class = RunningClass,
                   active_class_ns = RunningClassNS} = State)
                when is_binary(VarName) ->
    InstanceVar = Var#variable{idx = []},
    ObjRef = ephp_vars:get(Vars, InstanceVar, Ref),
    Context = ephp_object:get_context(ObjRef),
    {ok, Class} = ephp_class:get(State#state.class, RunningClassNS, RunningClass),
    {NewVar, NewState} =
        resolve_indexes(#variable{name = VarName, idx = Idx,
                                  type = object, class = Class#class.name,
                                  line = Line}, State),
    NewVarName = NewVar#variable.name,
    ClassAttr = ephp_class:get_attribute(Class, NewVarName),
    case ClassAttr of
        #class_attr{access = private} ->
            NewName = {private, NewVarName, RunningClassNS, RunningClass},
            {ephp_context:get(Context, NewVar#variable{class = RunningClass,
                                                       name = NewName,
                                                       type = object}),
                              NewState};
        _Data ->
            {ephp_context:get(Context, NewVar#variable{class = RunningClass,
                                                       type = object}), NewState}
    end;

resolve_var(#variable{type = normal} = Var, State) ->
    {NewVar, NewState} = resolve_indexes(Var, State),
    Value = ephp_vars:get(NewState#state.vars, NewVar, State#state.ref),
    {Value, NewState};

resolve_var(#variable{type = class, class = <<"self">>, line = Index},
            #state{active_class = <<>>}) ->
    ephp_error:error({error, enoclassscope, Index, ?E_ERROR, {<<"self">>}});

resolve_var(#variable{type = class, class = <<"self">>} = Var,
            #state{active_class = ClassName} = State) ->
    resolve_var(Var#variable{class = ClassName}, State);

%% TODO error if it's out of scope to use parent
resolve_var(#variable{type = class, class = <<"parent">>} = Var,
            #state{class = Classes,
                   active_class = ClassName,
                   active_class_ns = ClassNS} = State) ->
    %% TODO error if the parent is not defined
    {ok, #class{extends = ParentName, extends_ns = ParentNS}} =
        ephp_class:get(Classes, ClassNS, ClassName),
    resolve_var(Var#variable{class = ParentName, class_ns = ParentNS}, State);

resolve_var(#variable{type = class, class = ClassName, class_ns = ClassNS, line = Index} = Var,
            #state{class = Classes} = State) ->
    {NewVar, NewState} = resolve_indexes(Var, State),
    case ephp_class:get(Classes, ClassNS, ClassName) of
        {ok, #class{static_context = ClassCtx}} ->
            Value = get(ClassCtx, NewVar),
            {Value, NewState};
        {error, enoexist} ->
            ephp_error:error({error, eundefclass, Index, ?E_ERROR, {ClassNS, ClassName}})
    end.

% TODO complete list of casting and errors
resolve_cast(State, Line, Type, MemRef) when ?IS_MEM(MemRef) ->
    resolve_cast(State, Line, Type, ephp_mem:get(MemRef));
resolve_cast(#state{ref=Ctx}, Line, int, Value) ->
    ephp_data:to_int(Ctx, Line, Value);
resolve_cast(#state{ref=Ctx}, Line, float, Value) ->
    ephp_data:to_float(Ctx, Line, Value);
resolve_cast(#state{ref=Ctx}, Line, string, Value) ->
    ephp_data:to_bin(Ctx, Line, Value);
resolve_cast(_State, _Line, bool, Value) ->
    ephp_data:to_boolean(Value);
resolve_cast(_State, _Line, array, N) when
        is_number(N) orelse is_binary(N) orelse is_boolean(N) orelse
        N =:= infinity orelse N =:= nan ->
    ephp_array:store(auto, N, ephp_array:new());
resolve_cast(_State, _Line, array, Array) when ?IS_ARRAY(Array) ->
    Array;
resolve_cast(_State, _Line, array,
             #obj_ref{pid = Objects, ref = ObjectId}) ->
    #ephp_object{context = Ctx, class = Class} =
        ephp_object:get(Objects, ObjectId),
    lists:foldl(fun(#class_attr{name = Name}, Array) ->
        Value = ephp_context:get(Ctx, #variable{name = Name}),
        ephp_array:store(Name, Value, Array)
    end, ephp_array:new(), Class#class.attrs);
resolve_cast(_State, _Line, array, undefined) ->
    ephp_array:new();
resolve_cast(#state{ref = LocalCtx, class = Classes, global = GlobalCtx},
             Line, object, Array) when ?IS_ARRAY(Array) ->
    ClassName = <<"stdClass">>,
    ObjRef = ephp_class:instance(Classes, LocalCtx, GlobalCtx, [], ClassName, Line),
    VarVals = [ {#variable{name = K}, V} || {K, V} <- ephp_array:to_list(Array) ],
    ephp_object:set_bulk_attr(ObjRef, VarVals),
    ObjRef;
resolve_cast(#state{ref = LocalCtx, class = Classes, global = GlobalCtx},
             Line, object, N) when
        is_number(N) orelse is_binary(N) orelse is_boolean(N) orelse
        N =:= infinity orelse N =:= nan ->
    ClassName = <<"stdClass">>,
    ObjRef = ephp_class:instance(Classes, LocalCtx, GlobalCtx, [], ClassName, Line),
    ephp_object:set_attr(ObjRef, #variable{name = <<"scalar">>}, N),
    ObjRef;
resolve_cast(#state{ref = LocalCtx, class = Classes, global = GlobalCtx},
             Line, object, undefined) ->
    ClassName = <<"stdClass">>,
    ephp_class:instance(Classes, LocalCtx, GlobalCtx, [], ClassName, Line);
resolve_cast(_State, _Line, object, #obj_ref{} = Object) ->
    Object.

resolve_indexes(#variable{idx = Indexes} = Var, State) ->
    {NewIndexes, NewState} = lists:foldl(fun(Idx, {I, NS}) ->
        {Value, NState} = resolve(Idx, NS),
        {I ++ [Value], NState}
    end, {[], State}, Indexes),
    {Var#variable{idx = NewIndexes}, NewState}.

get_var_path(#variable{idx = []} = Var, _State) ->
    Var;

get_var_path(#variable{idx = Indexes} = Var, #state{vars = Vars} = State) ->
    NewIndexes = lists:foldl(fun
        (auto, LIdx) ->
            NewEntry = Var#variable{idx = LIdx},
            Value = case get_var_path_data(Vars, NewEntry, State#state.ref) of
                undefined ->
                    auto;
                Array when ?IS_ARRAY(Array) ->
                    auto;
                _Array ->
                    ephp_error:handle_error(State#state.ref, {error, enoarray,
                        Var#variable.line, State#state.active_file, ?E_WARNING, {}}),
                    throw({error, enoarray})
            end,
            LIdx ++ [Value];
        (Idx, LIdx) ->
            {Value, _Vars} = resolve(Idx, State),
            LIdx ++ [Value]
    end, [], Indexes),
    Var#variable{idx = NewIndexes}.

get_var_path_data(Vars, Entry, Ref) ->
    case ephp_vars:get(Vars, Entry, Ref) of
        MemRef when ?IS_MEM(MemRef) ->
            ephp_mem:get(MemRef);
        Other ->
            Other
    end.


resolve_txt(Texts, Line, State) ->
    lists:foldr(fun
        (true, {ResultTxt, NS}) ->
            {<<"1", ResultTxt/binary>>, NS};
        (Data, {ResultTxt, NS}) when
                Data =:= undefined orelse
                Data =:= false ->
            {<<ResultTxt/binary>>, NS};
        (Data, {ResultTxt, NS}) when is_binary(Data) ->
            {<<Data/binary, ResultTxt/binary>>, NS};
        (Data, {ResultTxt, NS}) when is_tuple(Data) ->
            {TextRaw, NewState} = resolve(Data, NS),
            Text = ephp_data:to_bin(NS#state.ref, Line, TextRaw),
            {<<Text/binary, ResultTxt/binary>>, NewState}
    end, {<<>>, State}, Texts).


resolve_op(#operation{type = Type,
                      expression_left = Op1,
                      expression_right = Op2}, State)
        when Type =:= 'and' orelse Type =:= 'or'->
    {RawOpRes1, State1} = resolve(Op1, State),
    OpRes1 = ephp_data:to_bool(RawOpRes1),
    case Type of
        'and' when OpRes1 =:= false ->
            {false, State1};
        'and' ->
            {OpRes2, State2} = resolve(Op2, State1),
            {ephp_data:to_bool(OpRes2), State2};
        'or' when OpRes1 =:= true ->
            {true, State1};
        'or' ->
            {OpRes2, State2} = resolve(Op2, State1),
            {ephp_data:to_bool(OpRes2), State2}
    end;

resolve_op(#operation{type = instanceof, expression_left = Op1,
                      expression_right = #constant{name = ClassName}},
           #state{ref = Ref} = State) ->
    {OpRes1, State1} = resolve(Op1, State),
    {ephp_data:instance_of(Ref, OpRes1, ClassName), State1};

resolve_op(#operation{type = Type, expression_left = Op1,
                      expression_right = Op2,
                      line = Index}, State) ->
    {OpRes1, State1} = resolve(Op1, State),
    {OpRes2, State2} = resolve(Op2, State1),
    {resolve_op(Type, OpRes1, OpRes2, Index, State2), State2};

resolve_op(Cond, State) ->
    {Value, NewState} = resolve(Cond, State),
    BoolValue = ephp_data:to_bool(Value),
    {BoolValue, NewState}.


resolve_op(<<"+">>, OpRes1, OpRes2, _Index, _State) when ?IS_ARRAY(OpRes1) andalso
                                                         ?IS_ARRAY(OpRes2) ->
    lists:foldl(fun({K,V}, A) ->
        case ephp_array:find(K, A) of
            error -> ephp_array:store(K, V, A);
            _ -> A
        end
    end, OpRes1, ephp_array:to_list(OpRes2));
resolve_op(<<"+">>, OpRes1, OpRes2, _Index, _State) ->
    ephp_data:zero_if_undef(OpRes1) + ephp_data:zero_if_undef(OpRes2);
resolve_op(<<"-">>, OpRes1, OpRes2, _Index, _State) ->
    ephp_data:zero_if_undef(OpRes1) - ephp_data:zero_if_undef(OpRes2);
resolve_op(<<"*">>, OpRes1, OpRes2, _Index, _State) ->
    ephp_data:zero_if_undef(OpRes1) * ephp_data:zero_if_undef(OpRes2);
resolve_op(<<"/">>, OpRes1, OpRes2, Index, State) ->
    A = ephp_data:zero_if_undef(OpRes1),
    B = ephp_data:zero_if_undef(OpRes2),
    if
        B == 0 ->
            #state{active_file = File} = State,
            Error = {error, edivzero, Index, File, ?E_WARNING, {}},
            ephp_error:handle_error(State#state.ref, Error);
        B == nan ->
            nan;
        B == infinity ->
            0;
        true ->
            A / B
    end;
resolve_op(<<"**">>, OpRes1, OpRes2, _Index, _State) ->
    ephp_data:pow(ephp_data:zero_if_undef(OpRes1),
                  ephp_data:zero_if_undef(OpRes2));
resolve_op(<<"%">>, OpRes1, OpRes2, _Index, _State) ->
    trunc(ephp_data:zero_if_undef(OpRes1)) rem
    trunc(ephp_data:zero_if_undef(OpRes2));
resolve_op(<<"<">>, undefined, _OpRes2, _Index, _State) -> true;
resolve_op(<<"<">>, _OpRes1, undefined, _Index, _State) -> false;
resolve_op(<<"<">>, OpRes1, OpRes2, _Index, _State) -> OpRes1 < OpRes2;
resolve_op(<<">">>, undefined, _OpRes2, _Index, _State) -> false;
resolve_op(<<">">>, _OpRes1, undefined, _Index, _State) -> true;
resolve_op(<<">">>, OpRes1, OpRes2, _Index, _State) -> OpRes1 > OpRes2;
resolve_op(<<">=">>, undefined, undefined, _Index, _State) -> true;
resolve_op(<<">=">>, undefined, _OpRes2, _Index, _State) -> true;
resolve_op(<<">=">>, _OpRes1, undefined, _Index, _State) -> false;
resolve_op(<<">=">>, OpRes1, OpRes2, _Index, _State) -> OpRes1 >= OpRes2;
resolve_op(<<"=<">>, undefined, undefined, _Index, _State) -> true;
resolve_op(<<"=<">>, undefined, _OpRes2, _Index, _State) -> false;
resolve_op(<<"=<">>, _OpRes1, undefined, _Index, _State) -> true;
resolve_op(<<"=<">>, OpRes1, OpRes2, _Index, _State) -> OpRes1 =< OpRes2;
resolve_op(<<"==">>, OpRes1, OpRes2, _Index, _State) when ?IS_OBJECT(OpRes1) andalso
                                                          ?IS_OBJECT(OpRes2) ->
    ephp_object:get_class_name(OpRes1) =:= ephp_object:get_class_name(OpRes2);
resolve_op(<<"==">>, OpRes1, OpRes2, _Index, _State) ->
    ephp_data:is_equal(OpRes1, OpRes2);
resolve_op(<<"===">>, OpRes1, OpRes2, _Index, _State) -> OpRes1 =:= OpRes2;
resolve_op(<<"!=">>, OpRes1, OpRes2, _Index, _State) -> OpRes1 /= OpRes2;
resolve_op(<<"!==">>, OpRes1, OpRes2, _Index, _State) -> OpRes1 =/= OpRes2;
resolve_op(<<"^">>, OpRes1, OpRes2, _Index, _State) ->
    ephp_data:zero_if_undef(OpRes1) bxor ephp_data:zero_if_undef(OpRes2);
resolve_op(<<"|">>, OpRes1, OpRes2, _Index, _State) ->
    ephp_data:zero_if_undef(OpRes1) bor ephp_data:zero_if_undef(OpRes2);
resolve_op(<<"&">>, OpRes1, OpRes2, _Index, _State) ->
    ephp_data:zero_if_undef(OpRes1) band ephp_data:zero_if_undef(OpRes2);
resolve_op(instanceof, OpRes1, OpRes2, _Index, _State) ->
    ephp_object:get_class_name(OpRes1) =:= ephp_object:get_class_name(OpRes2).

-spec load_state(context()) -> #state{}.

load_state(Context) ->
    erlang:get(Context).

-spec save_state(#state{}) -> #state{} | undefined.

save_state(#state{ref=Ref} = State) ->
    erlang:put(Ref, State).

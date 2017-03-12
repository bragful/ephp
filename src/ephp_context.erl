-module(ephp_context).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-record(state, {
    ref :: ephp:context_id(),
    vars :: ephp:vars_id(),
    funcs :: ephp:funcs_id(),
    class :: ephp:classes_id(),
    timezone = "Europe/Madrid" :: string(),
    output :: ephp:output_id(),
    const :: ephp:consts_id(),
    global :: ephp:context_id(),
    include :: ephp:includes_id(),
    shutdown :: ephp:shutdown_id(),
    errors :: ephp:errors_id(),
    meta = [] :: term(),

    active_file = <<>> :: file_name(),
    active_fun = <<>> :: function_name(),
    active_fun_args = 0 :: non_neg_integer(),
    active_class = <<>> :: class_name()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    get/2,
    set/3,
    solve/2,
    destroy/1,
    destroy_all/1,

    get_state/1,

    get_active_file/1,
    set_active_file/2,

    set_tz/2,
    get_tz/1,

    get_output/1,
    set_output/2,
    set_output_handler/2,
    get_output_handler/1,

    register_func/3,
    register_func/4,
    register_func/5,
    get_functions/1,
    get_function/2,

    get_current_function/1,
    get_current_class/1,
    get_current_function_arity/1,

    set_errors_id/2,
    get_errors_id/1,

    get_const/3,
    register_const/3,

    load/2,
    load_once/2,

    call_method/3,
    register_class/2,
    set_class_alias/3,

    set_global/2,
    generate_subcontext/1,

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
    Ref = make_ref(),
    {ok, Funcs} = ephp_func:start_link(),
    {ok, Output} = ephp_output:start_link(Ref),
    {ok, Const} = ephp_const:start_link(),
    {ok, Inc} = ephp_include:start_link(),
    {ok, Class} = ephp_class:start_link(),
    {ok, Shutdown} = ephp_shutdown:start_link(),
    {ok, Errors} = ephp_error:start_link(),
    start_link(#state{
        ref = Ref,
        output = Output,
        funcs = Funcs,
        class = Class,
        const = Const,
        include = Inc,
        shutdown = Shutdown,
        errors = Errors
    }).

start_link(#state{ref=undefined}=State) ->
    start_link(State#state{ref=make_ref()});

start_link(#state{ref=Ref, global=Ref}) when is_reference(Ref) ->
    throw({error, ecyclerefs});

start_link(#state{ref=Ref}=State) when is_reference(Ref) ->
    {ok, Vars} = ephp_vars:start_link(),
    save_state(State#state{
        ref = Ref,
        vars = Vars
    }),
    {ok, Ref}.

start_mirror(#state{}=State) ->
    Ref = make_ref(),
    save_state(State#state{ref=Ref}),
    {ok, Ref}.

get(Context, VarPath) ->
    #state{vars=Vars} = load_state(Context),
    ephp_vars:get(Vars, VarPath, Context).

set(Context, VarPath, Value) ->
    State = load_state(Context),
    ephp_vars:set(State#state.vars, get_var_path(VarPath, State), Value),
    ok.

get_meta(Context, Key) ->
    #state{meta=Meta} = load_state(Context),
    case lists:keyfind(Key, 1, Meta) of
        false -> undefined;
        {Key, Value} -> Value
    end.

set_meta(Context, Key, Value) ->
    #state{meta=Meta} = State = load_state(Context),
    NewMeta = lists:keystore(Key, 1, Meta, {Key, Value}),
    save_state(State#state{meta=NewMeta}),
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
    ephp_output:destroy(State#state.output),
    ephp_const:destroy(State#state.const),
    ephp_include:destroy(State#state.include),
    ephp_func:destroy(State#state.funcs),
    ephp_error:destroy(State#state.errors),
    ephp_class:destroy(State#state.class),
    ephp_vars:destroy(State#state.vars),
    ephp_shutdown:destroy(State#state.shutdown),
    destroy(Context).

get_state(Context) ->
    get(Context).

register_func(Context, PHPFunc, Module, Fun, PackArgs)
        when is_atom(Module) andalso is_atom(Fun) ->
    #state{funcs=Funcs} = load_state(Context),
    ephp_func:register_func(Funcs, PHPFunc, Module, Fun, PackArgs),
    ok;

register_func(Context, PHPFunc, Args, Code, PackArgs) ->
    #state{funcs=Funcs} = load_state(Context),
    ephp_func:register_func(Funcs, PHPFunc, Args, Code, PackArgs),
    ok.

register_func(Context, PHPFunc, Module, Fun)
        when is_atom(Module) andalso is_atom(Fun) ->
    #state{funcs=Funcs} = load_state(Context),
    ephp_func:register_func(Funcs, PHPFunc, Module, Fun),
    ok;

register_func(Context, PHPFunc, Args, Code) ->
    #state{funcs=Funcs} = load_state(Context),
    ephp_func:register_func(Funcs, PHPFunc, Args, Code),
    ok.

register_func(Context, PHPFunc, Fun) ->
    #state{funcs=Funcs} = load_state(Context),
    ephp_func:register_func(Funcs, PHPFunc, Fun),
    ok.

get_functions(Context) ->
    #state{funcs=Funcs} = load_state(Context),
    ephp_func:get_functions(Funcs).

get_function(Context, FuncName) ->
    #state{funcs=Funcs} = load_state(Context),
    ephp_func:get(Funcs, FuncName).

get_current_function(Context) ->
    #state{active_fun=ActiveFun} = load_state(Context),
    ActiveFun.

get_current_function_arity(Context) ->
    #state{active_fun_args=ActiveFunArgs} = load_state(Context),
    ActiveFunArgs.

get_current_class(Context) ->
    #state{active_class=ActiveClass} = load_state(Context),
    ActiveClass.

get_errors_id(Context) ->
    #state{errors=Errors} = load_state(Context),
    Errors.

set_errors_id(Context, Errors) ->
    State = load_state(Context),
    save_state(State#state{errors=Errors}),
    ok.

get_const(Context, Name, Index) ->
    #state{const=Const} = load_state(Context),
    ephp_const:get(Const, Name, Index, Context).

register_const(Context, Name, Value) ->
    #state{const=Const} = load_state(Context),
    ephp_const:set(Const, Name, Value),
    ok.

call_method(Context, Instance, Call) ->
    {Val, NS} = run_method(Instance, Call, load_state(Context)),
    save_state(NS),
    Val.

get_active_file(Context) ->
    #state{active_file=Filename} = load_state(Context),
    Filename.

set_active_file(Context, undefined) ->
    Filename = <<"php shell code">>,
    {ok, Cwd} = file:get_cwd(),
    #state{const=Const} = State = load_state(Context),
    save_state(State#state{active_file=Filename}),
    ephp_const:set_bulk(Const, [
        {<<"__FILE__">>, Filename},
        {<<"__DIR__">>, list_to_binary(Cwd)}
    ]),
    ok;

set_active_file(Context, Filename) ->
    #state{const=Const} = State = load_state(Context),
    save_state(State#state{active_file=Filename}),
    ephp_const:set_bulk(Const, [
        {<<"__FILE__">>, Filename},
        {<<"__DIR__">>, filename:dirname(Filename)}
    ]),
    ok.

get_tz(Context) ->
    #state{timezone=TZ} = load_state(Context),
    TZ.

set_tz(Context, TZ) ->
    State = load_state(Context),
    case ephp_timezone:is_valid(TZ) of
        true ->
            save_state(State#state{timezone=TZ}),
            true;
        false ->
            false
    end.

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
    #state{include=Inc} = load_state(Context),
    ephp_include:load(Inc, File).

load_once(Context, File) ->
    #state{include=Inc} = load_state(Context),
    ephp_include:load_once(Inc, File).

register_class(Context, Class) ->
    #state{class=Classes, global=GlobalCtx} = load_state(Context),
    ephp_class:register_class(Classes, GlobalCtx, Class),
    ok.

set_class_alias(Context, ClassName, ClassAlias) ->
    #state{class=Classes} = load_state(Context),
    ephp_class:set_alias(Classes, ClassName, ClassAlias).

set_global(Context, GlobalContext) ->
    State = load_state(Context),
    save_state(State#state{global=GlobalContext}),
    ok.

generate_subcontext(Context) ->
    State = load_state(Context),
    start_link(State#state{
        ref=undefined,
        global=Context}).

register_shutdown_func(Context, FuncName) ->
    #state{shutdown=Ref} = load_state(Context),
    ephp_shutdown:register_func(Ref, FuncName).

unregister_shutdown_func(Context, FuncName) ->
    #state{shutdown=Ref} = load_state(Context),
    ephp_shutdown:unregister_func(Ref, FuncName).

get_shutdown_funcs(Context) ->
    #state{shutdown=Ref} = load_state(Context),
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
resolve(#assign{
        variable=#variable{type=normal}=Var,
        expression=#ref{var=RefVar}}, State) ->
    ephp_vars:ref(State#state.vars, Var, State#state.vars, RefVar),
    resolve(RefVar, State);

resolve(#assign{variable=#variable{name = <<"this">>, idx=[]}}=Assign, State) ->
    ephp_error:error({error, eassignthis, Assign#assign.line,
        ?E_ERROR, State#state.active_file});

resolve(#assign{variable=#variable{type=normal}=Var,expression=Expr}, State) ->
    {Value, NState} = resolve(Expr, State),
    case catch get_var_path(Var, NState) of
        #variable{}=VarPath ->
            ephp_vars:set(NState#state.vars, VarPath, Value),
            {Value, NState};
        {error, _Reason} ->
            {undefined, State}
    end;

resolve(#assign{variable=#variable{type=class, class = <<"self">>, line=Index}},
        #state{active_class=undefined,active_file=File}) ->
    ephp_error:error({error, eundefclass, Index, ?E_ERROR, {File, <<"self">>}});

resolve(#assign{variable=#variable{type=class, class = <<"self">>}=Var}=Assign,
        #state{active_class=ClassName}=State) ->
    resolve(Assign#assign{variable=Var#variable{class=ClassName}}, State);

resolve(#assign{
            variable=#variable{type=class,class=ClassName,line=Index}=Var,
            expression=Expr},
        #state{class=Classes,active_file=File}=State) ->
    case catch get_var_path(Var, State) of
        #variable{}=VarPath ->
            {Value, NState} = resolve(Expr, State),
            case ephp_class:get(Classes, ClassName) of
            {ok, #class{static_context=ClassCtx}} ->
                set(ClassCtx, VarPath, Value);
            {error, enoexist} ->
                ephp_error:error({error, eundefclass, Index,
                    ?E_ERROR, {File, ClassName}})
            end,
            {Value, NState};
        {error, _Reason} ->
            {undefined, State}
    end;

resolve(#operation{}=Op, State) ->
    resolve_op(Op, State);

resolve(#int{int=Int}, State) ->
    {Int, State};

resolve(N, State) when is_number(N) ->
    {N, State};

resolve(S, State) when is_binary(S) ->
    {S, State};

resolve(A, State) when ?IS_ARRAY(A) ->
    {A, State};

resolve(#float{float=Float}, State) ->
    {Float, State};

resolve(#text{text=Text}, State) ->
    {Text, State};

resolve(#text_to_process{text=Texts, line=Line}, State) ->
    resolve_txt(Texts, Line, State);

resolve({pre_incr, Var, _Line}, State) ->
    case catch get_var_path(Var, State) of
        #variable{}=VarPath ->
            case ephp_vars:get(State#state.vars, VarPath, State#state.ref) of
                undefined ->
                    ephp_vars:set(State#state.vars, VarPath, 1),
                    {1, State};
                V when is_number(V) ->
                    ephp_vars:set(State#state.vars, VarPath, V+1),
                    {V+1, State};
                V when is_binary(V) andalso byte_size(V) > 0 ->
                    NewVal = try
                        binary_to_integer(V) + 1
                    catch error:badarg ->
                        ephp_data:increment_code(V)
                    end,
                    ephp_vars:set(State#state.vars, VarPath, NewVal),
                    {NewVal, State};
                V ->
                    {V, State}
            end;
        {error, _Reason} ->
            {undefined, State}
    end;

resolve({pre_decr, Var, _Line}, State) ->
    VarPath = get_var_path(Var, State),
    case ephp_vars:get(State#state.vars, VarPath, State#state.ref) of
        undefined ->
            {undefined, State};
        V when is_number(V) ->
            ephp_vars:set(State#state.vars, VarPath, V-1),
            {V-1, State};
        V ->
            {V, State}
    end;

resolve({post_incr, Var, _Line}, State) ->
    VarPath = get_var_path(Var, State),
    case ephp_vars:get(State#state.vars, VarPath, State#state.ref) of
        undefined ->
            ephp_vars:set(State#state.vars, VarPath, 1),
            {undefined, State};
        V when is_number(V) ->
            ephp_vars:set(State#state.vars, VarPath, V+1),
            {V, State};
        V when is_binary(V) andalso byte_size(V) > 0 ->
            NewVal = try
                binary_to_integer(V) + 1
            catch error:badarg ->
                ephp_data:increment_code(V)
            end,
            ephp_vars:set(State#state.vars, VarPath, NewVal),
            {V, State};
        V ->
            {V, State}
    end;

resolve({post_decr, Var, _Line}, State) ->
    VarPath = get_var_path(Var, State),
    case ephp_vars:get(State#state.vars, VarPath, State#state.ref) of
        undefined ->
            {undefined, State};
        V when is_number(V) ->
            ephp_vars:set(State#state.vars, VarPath, V-1),
            {V, State};
        V ->
            {V, State}
    end;

resolve({operation_minus, Expr, Line}, #state{ref=Ctx}=State) ->
    case resolve(Expr, State) of
        {Number, NewState} when is_number(Number) ->
            {-Number, NewState};
        {Binary, NewState} when is_binary(Binary) ->
            {-ephp_data:bin_to_number(Binary), NewState};
        {Array, _NewState} when ?IS_ARRAY(Array) ->
            File = State#state.active_file,
            ephp_error:error({error, eunsupportop, Line, ?E_ERROR, File});
        {#reg_instance{class=#class{name=ClassName}}, NewState} ->
            File = State#state.active_file,
            Data = {File, ClassName, <<"int">>},
            ephp_error:handle_error(Ctx, {error, enocast, Line, ?E_NOTICE, Data}),
            {-1, NewState}
    end;

resolve({operation_not, Expr, _Line}, State) ->
    EmptyArray = ephp_array:new(),
    case resolve(Expr, State) of
        {false, NewState} -> {true, NewState};
        {<<>>, NewState} -> {true, NewState};
        {0, NewState} -> {true, NewState};
        {<<"0">>, NewState} -> {true, NewState};
        {EmptyArray, NewState} -> {true, NewState};
        {undefined, NewState} -> {true, NewState};
        {_, NewState} -> {false, NewState}
    end;

resolve({operation_bnot, Expr, Line}, State) ->
    case resolve(Expr, State) of
        {Number, NewState} when is_number(Number) -> {bnot(Number), NewState};
        {Binary, NewState} when is_binary(Binary) ->
            {<< <<bnot(B)/integer>> || <<B:8/integer>> <= Binary >>, NewState};
        _ ->
            File = State#state.active_file,
            ephp_error:error({error, eunsupportop, Line, ?E_ERROR, File})
    end;

resolve(#if_block{conditions=Cond}=IfBlock, State) ->
    case resolve_op(Cond, State) of
    {true,NewState} ->
        resolve(IfBlock#if_block.true_block, NewState);
    {false,NewState} ->
        resolve(IfBlock#if_block.false_block, NewState)
    end;

resolve(#variable{}=Var, State) ->
    resolve_var(Var, State);

resolve(#array{elements=ArrayElements}, State) ->
    {Array,NState} = lists:foldl(fun
        (#array_element{idx=auto,element=Element}, {Dict,NS}) ->
            {Value, NewState} = resolve(Element,NS),
            {ephp_array:store(auto,Value,Dict),NewState};
        (#array_element{idx=I, element=Element}, {Dict,NS}) ->
            {Value, NewState} = resolve(Element,NS),
            {Idx, ReNewState} = resolve(I,NewState),
            {ephp_array:store(Idx,Value,Dict), ReNewState}
    end, {ephp_array:new(),State}, ArrayElements),
    {Array, NState};

resolve(#concat{texts=Texts, line=Line}, State) ->
    resolve_txt(Texts, Line, State);

resolve(#call{name=#function{args=RawFuncArgs,code=Code,use=Use},args=RawArgs},
        #state{ref=Ref,vars=Vars,const=Const}=State) ->
    {Args, NStatePrev} = resolve_args(RawArgs, State),
    {FuncArgs, NState} = resolve_func_args(RawFuncArgs, NStatePrev),
    {ok, NewVars} = ephp_vars:start_link(),
    {ok, SubContext} = start_mirror(NState#state{
        vars=NewVars,
        global=Ref,
        active_fun = ?FUNC_ANON_NAME,
        active_fun_args = length(RawArgs)}),
    ephp_vars:zip_args(Vars, NewVars, Args, FuncArgs),
    lists:foreach(fun
        ({#variable{}=K,V}) ->
            ephp_vars:set(NewVars, K, V);
        ({#var_ref{pid=NVars, ref=V},N}) ->
            ephp_vars:ref(NewVars, N, NVars, V)
    end, Use),
    register_superglobals(Ref, NewVars),
    ephp_const:set(Const, <<"__FUNCTION__">>, ?FUNC_ANON_NAME),
    Value = case ephp_interpr:run(SubContext, #eval{statements=Code}) of
        {return, V} -> V;
        _ -> undefined
    end,
    destroy(SubContext),
    ephp_vars:destroy(NewVars),
    ephp_const:set(Const, <<"__FUNCTION__">>, State#state.active_fun),
    {Value, NState};

resolve(#call{name=Fun}=Call, State) when not is_binary(Fun) ->
    {Name, NewState} = resolve(Fun, State),
    resolve(Call#call{name=Name}, NewState);

resolve(#call{type=normal,name=Fun,args=RawArgs,line=Index}=_Call,
        #state{ref=Ref,vars=Vars,funcs=Funcs,const=Const}=State) ->
    case ephp_func:get(Funcs, Fun) of
    {ok,#reg_func{type=builtin, pack_args=PackArgs, builtin={M,F}}}
            when is_atom(M) andalso is_atom(F) ->
        SState = case {M,F} of
            {ephp_lib_vars, isset} ->
                % FIXME: add config for silent Notice when a var isn't defined
                State#state{ref=undefined};
            _ ->
                State
        end,
        {Args, NState} = resolve_args(RawArgs, SState),
        save_state(NState),
        Value = if
            PackArgs -> erlang:apply(M,F,[Ref,Index,Args]);
            true -> erlang:apply(M,F,[Ref,Index|Args])
        end,
        {Value, load_state(Ref)};
    {ok,#reg_func{type=builtin, pack_args=PackArgs, builtin=F}}
            when is_function(F) ->
        {Args, NState} = resolve_args(RawArgs, State),
        save_state(NState),
        Value = if
            PackArgs -> F([Ref,Index,Args]);
            true -> F([Ref,Index|Args])
        end,
        {Value, load_state(Ref)};
    {ok,#reg_func{type=php, args=RawFuncArgs, code=Code}} ->
        {Args, NStatePrev} = resolve_args(RawArgs, State),
        {FuncArgs, NState} = resolve_func_args(RawFuncArgs, NStatePrev),
        {ok, NewVars} = ephp_vars:start_link(),
        {ok, SubContext} = start_mirror(NState#state{
            vars=NewVars,
            global=Ref,
            active_fun=Fun,
            active_class=undefined,
            active_fun_args=length(Args)}),
        ephp_vars:zip_args(Vars, NewVars, Args, FuncArgs),
        register_superglobals(Ref, NewVars),
        ephp_const:set(Const, <<"__FUNCTION__">>, Fun),
        Value = case ephp_interpr:run(SubContext, #eval{statements=Code}) of
            {return, V} -> V;
            _ -> undefined
        end,
        destroy(SubContext),
        ephp_vars:destroy(NewVars),
        ephp_const:set(Const, <<"__FUNCTION__">>, State#state.active_fun),
        {Value, NState};
    error ->
        File = State#state.active_file,
        ephp_error:error({error, eundefun, Index, ?E_ERROR, {File, Fun}})
    end;

resolve(#call{type=class,class=Name,line=Index}=Call,
        #state{class=Classes}=State) ->
    case ephp_class:get(Classes, Name) of
    {ok, Class} ->
        run_method(Class, Call, State);
    {error, enoexist} ->
        ephp_error:error({error, eundefclass, Index, ?E_ERROR, Name})
    end;

resolve({object,Idx,Line}, State) ->
    {{object,Idx,Line}, State};

resolve(#instance{name=ClassName, args=RawArgs, line=Line}=Instance,
        #state{ref=LocalCtx,class=Classes,global=GlobalCtx}=State) ->
    Value = ephp_class:instance(Classes, LocalCtx, GlobalCtx, ClassName, Line),
    RetInstance = Value#reg_instance{instance = Instance},
    #reg_instance{class = Class} = RetInstance,
    case ephp_class:get_constructor(Class) of
    undefined ->
        {RetInstance, State};
    #class_method{name = ConstructorName} ->
        Call = #call{type = object, name = ConstructorName, args=RawArgs},
        {_, NState} = run_method(RetInstance, Call, State),
        {RetInstance, NState}
    end;

resolve({global, _Var,_Line}, #state{global=undefined}=State) ->
    {undefined, State};

resolve({global, GVars,_Line}, #state{
        global=GlobalCtx,
        vars=Vars}=State) ->
    #state{vars=GlobalVars} = load_state(GlobalCtx),
    lists:foreach(fun(GlobalVar) ->
        ephp_vars:ref(Vars, GlobalVar, GlobalVars, GlobalVar)
    end, GVars),
    {undefined, State};

resolve(#constant{type=class,class=ClassName,name=Name},
        #state{class=Classes}=State) ->
    {ephp_class:get_const(Classes, ClassName, Name), State};

resolve(#constant{type=normal,name=Name,line=Line},
        #state{ref=Ref, const=Const}=State) ->
    {ephp_const:get(Const, Name, Line, Ref), State};

resolve(#print_text{text=Text}, #state{output=Output}=State) ->
    ephp_output:push(Output, Text),
    {1, State};

resolve(undefined, State) ->
    {undefined, State};

resolve({ref, Var, _Line}, #state{vars=Vars}=State) ->
    {{var_ref, Vars, Var}, State};

resolve(auto, _State) ->
    ephp_error:error({error, earrayundef, undefined, ?E_ERROR, <<>>});

resolve({silent, Statement}, #state{errors=Errors}=State) ->
    ephp_error:run_quiet(Errors, fun() ->
        resolve(Statement, State)
    end);

resolve(#function{name=undefined,use=Use}=Anon, #state{vars=Vars}=State) ->
    {NewUse, NState} = lists:foldl(fun
        (#variable{}=K, {Acc, S}) ->
            {V,NewState} = resolve(K, S),
            {Acc ++ [{K,V}], NewState};
        (#ref{var=#variable{}=V}, {Acc, S}) ->
            {Acc ++ [{{var_ref,Vars,V},V}], S}
    end, {[], State}, Use),
    {Anon#function{use=NewUse}, NState};

resolve(#cast{type=Type, content=C, line=Line}, State) ->
    {Value, NState} = resolve(C, State),
    {resolve_cast(State, Line, Type, Value), NState};

resolve(Unknown, _State) ->
    ephp_error:error({error, eundeftoken, undefined, ?E_CORE_ERROR, Unknown}).


register_superglobals(GlobalCtx, Vars) ->
    #state{vars=GlobalVars} = load_state(GlobalCtx),
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
    lists:foreach(fun(GlobalName) ->
        GlobalVar = #variable{name = GlobalName},
        ephp_vars:ref(Vars, GlobalVar, GlobalVars, GlobalVar)
    end, SuperGlobals).

resolve_func_args(RawFuncArgs, State) ->
    lists:foldl(fun
        (#variable{default_value=Val}=Var, {Vars, S}) when Val =/= null ->
            {Value, NewState} = resolve(Val,S),
            {Vars ++ [Var#variable{default_value=Value}], NewState};
        (Var, {Vars, NewState}) ->
            {Vars ++ [Var], NewState}
    end, {[], State}, RawFuncArgs).

resolve_args(RawArgs, State) ->
    lists:foldl(fun(Arg, {Args,S}) ->
        {A,NewState} = resolve(Arg,S),
        {Args ++ [{Arg,A}], NewState}
    end, {[], State}, RawArgs).

run_method(RegInstance, #call{args=RawArgs}=Call,
        #state{ref=Ref, const=Const, vars=Vars}=State) ->
    {Args, NStatePrev} = resolve_args(RawArgs, State),
    {ok, NewVars} = ephp_vars:start_link(),
    Class = case RegInstance of
    #reg_instance{class=C} ->
        ephp_vars:set(NewVars, #variable{name = <<"this">>}, RegInstance),
        C;
    #class{}=C ->
        C
    end,
    #class_method{name=MethodName, args=RawMethodArgs, code=Code} =
        ephp_class:get_method(Class, Call#call.name),
    {MethodArgs, NState} = resolve_func_args(RawMethodArgs, NStatePrev),
    {ok, SubContext} = start_mirror(NState#state{
        vars=NewVars,
        global=Ref,
        active_fun=MethodName,
        active_fun_args=length(Args),
        active_class=Class#class.name}),
    ephp_vars:zip_args(Vars, NewVars, Args, MethodArgs),
    register_superglobals(Ref, NewVars),
    ephp_const:set_bulk(Const, [
        {<<"__FUNCTION__">>, MethodName},
        {<<"__CLASS__">>, Class#class.name}
    ]),
    Value = case ephp_interpr:run(SubContext, #eval{statements=Code}) of
        {return, V} -> V;
        _ -> undefined
    end,
    destroy(SubContext),
    ephp_vars:destroy(NewVars),
    ephp_const:set_bulk(Const, [
        {<<"__FUNCTION__">>, State#state.active_fun},
        {<<"__CLASS__">>, State#state.active_class}
    ]),
    {Value, NState}.

resolve_var(#variable{type=normal,idx=[]}=Var, State) ->
    {ephp_vars:get(State#state.vars, Var, State#state.ref), State};

resolve_var(#variable{name = <<"this">>, idx=[{object,#call{}=Call,_}]}=Var, State) ->
    InstanceVar = Var#variable{idx=[]},
    Instance = ephp_vars:get(State#state.vars, InstanceVar, State#state.ref),
    run_method(Instance, Call#call{type=object}, State);

resolve_var(#variable{idx=[{object,#call{}=Call,_}]}=Var, State) ->
    InstanceVar = Var#variable{idx=[]},
    #reg_instance{class=Class} = Instance =
        ephp_vars:get(State#state.vars, InstanceVar, State#state.ref),
    case ephp_class:get_method(Class, Call#call.name) of
        #class_method{access=public} ->
            run_method(Instance, Call#call{type=object}, State);
        #class_method{access=protected} ->
            File = State#state.active_file,
            Data = {File, Class#class.name, Call#call.name, <<"protected">>},
            ephp_error:error({error, ecallprivate, Var#variable.line,
                ?E_ERROR, Data});
        #class_method{access=private} ->
            File = State#state.active_file,
            Data = {File, Class#class.name, Call#call.name, <<"private">>},
            ephp_error:error({error, ecallprivate, Var#variable.line,
                ?E_ERROR, Data})
    end;

resolve_var(#variable{idx=[{object,#variable{}=SubVar,_Line}|Idx]}=Var,State) ->
    #reg_instance{class=Class, context=Context} =
        ephp_vars:get(State#state.vars, Var#variable{idx=[]}, State#state.ref),
    {SubVal, State2} = resolve(SubVar, State),
    {NewVar, State3} =
        resolve_indexes(#variable{name=SubVal,idx=Idx}, State2),
    case ephp_class:get_attribute(Class, SubVal) of
        #class_attr{access=public} ->
            {ephp_context:get(Context, NewVar), State3};
        #class_attr{access=protected} ->
            File = State#state.active_file,
            Data = {File, Class#class.name, SubVal, <<"protected">>},
            ephp_error:error({error, eprivateaccess, SubVar#variable.line,
                ?E_ERROR, Data});
        #class_attr{access=private} ->
            File = State#state.active_file,
            Data = {File, Class#class.name, SubVal, <<"private">>},
            ephp_error:error({error, eprivateaccess, SubVar#variable.line,
                ?E_ERROR, Data})
    end;

resolve_var(#variable{idx=[{object,VarName,_Line}|Idx]}=Var, State)
        when is_binary(VarName) ->
    #reg_instance{class=Class, context=Context} =
        ephp_vars:get(State#state.vars, Var#variable{idx=[]}, State#state.ref),
    {NewVar, NewState} =
        resolve_indexes(#variable{name=VarName,idx=Idx}, State),
    ClassAttr = ephp_class:get_attribute(Class, NewVar#variable.name),
    case ClassAttr of
        #class_attr{access=public} ->
            {ephp_context:get(Context, NewVar), NewState};
        #class_attr{access=protected} ->
            File = State#state.active_file,
            Data = {File, Class#class.name,
                NewVar#variable.name, <<"protected">>},
            ephp_error:error({error, eprivateaccess, Var#variable.line,
                ?E_ERROR, Data});
        #class_attr{access=private} ->
            File = State#state.active_file,
            Data = {File, Class#class.name,
                NewVar#variable.name, <<"private">>},
            ephp_error:error({error, eprivateaccess, Var#variable.line,
                ?E_ERROR, Data});
        undefined -> % dynamic attribute, not defined
            {ephp_context:get(Context, NewVar), NewState}
    end;

resolve_var(#variable{type=normal}=Var, State) ->
    {NewVar, NewState} = resolve_indexes(Var, State),
    Value = ephp_vars:get(NewState#state.vars, NewVar, State#state.ref),
    {Value, NewState};

resolve_var(#variable{type=class,class = <<"self">>, line=Index},
        #state{active_class=undefined}) ->
    ephp_error:error({error, enoclassscope, Index, ?E_ERROR, <<"self">>});

resolve_var(#variable{type=class, class = <<"self">>}=Var,
        #state{active_class=ClassName}=State) ->
    resolve_var(Var#variable{class=ClassName}, State);

resolve_var(#variable{type=class,class=ClassName,line=Index}=Var,
        #state{class=Classes,active_file=File}=State) ->
    {NewVar, NewState} = resolve_indexes(Var, State),
    case ephp_class:get(Classes, ClassName) of
    {ok, #class{static_context=ClassCtx}} ->
        Value = get(ClassCtx, NewVar),
        {Value, NewState};
    {error, enoexist} ->
        ephp_error:error({error, eundefclass, Index, ?E_ERROR,
            {File,ClassName}})
    end.

% TODO complete list of casting and errors
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
             #reg_instance{class=Class, context=Ctx}) ->
    lists:foldl(fun(#class_attr{name=Name}, Array) ->
        Value = ephp_context:get(Ctx, #variable{name=Name}),
        ephp_array:store(Name, Value, Array)
    end, ephp_array:new(), Class#class.attrs);
resolve_cast(_State, _Line, array, undefined) ->
    ephp_array:new();
resolve_cast(#state{ref=LocalCtx,class=Classes,global=GlobalCtx},
             Line, object, Array) when ?IS_ARRAY(Array) ->
    ClassName = <<"stdClass">>,
    Val = ephp_class:instance(Classes, LocalCtx, GlobalCtx, ClassName, Line),
    #reg_instance{context=Ctx} = Val,
    NewClass = ephp_array:fold(fun(K, V, Class) ->
        ephp_context:set(Ctx, #variable{name=K}, V),
        ephp_class:add_if_no_exists_attrib(Class, K)
    end, Val#reg_instance.class, Array),
    Val#reg_instance{class=NewClass};
resolve_cast(#state{ref=LocalCtx,class=Classes,global=GlobalCtx},
             Line, object, N) when
        is_number(N) orelse is_binary(N) orelse is_boolean(N) orelse
        N =:= infinity orelse N =:= nan ->
    ClassName = <<"stdClass">>,
    Val = ephp_class:instance(Classes, LocalCtx, GlobalCtx, ClassName, Line),
    #reg_instance{context=Ctx, class=Class} = Val,
    ephp_context:set(Ctx, #variable{name = <<"scalar">>}, N),
    NewClass = ephp_class:add_if_no_exists_attrib(Class, <<"scalar">>),
    Val#reg_instance{class=NewClass};
resolve_cast(#state{ref=LocalCtx,class=Classes,global=GlobalCtx},
             Line, object, undefined) ->
    ClassName = <<"stdClass">>,
    ephp_class:instance(Classes, LocalCtx, GlobalCtx, ClassName, Line);
resolve_cast(_State, _Line, object, #reg_instance{}=Object) ->
    Object.

resolve_indexes(#variable{idx=Indexes}=Var, State) ->
    {NewIndexes, NewState} = lists:foldl(fun(Idx,{I,NS}) ->
        {Value, NState} = resolve(Idx, NS),
        {I ++ [Value], NState}
    end, {[],State}, Indexes),
    {Var#variable{idx=NewIndexes}, NewState}.

get_var_path(#variable{idx=[]}=Var, _State) ->
    Var;

get_var_path(#variable{idx=Indexes}=Var, #state{vars=Vars}=State) ->
    NewIndexes = lists:foldl(fun
        (auto, LIdx) ->
            NewEntry = Var#variable{idx=LIdx},
            Value = case ephp_vars:get(Vars, NewEntry, State#state.ref) of
            undefined ->
                auto;
            Array when ?IS_ARRAY(Array) ->
                auto;
            _Array ->
                ephp_error:handle_error(State#state.ref, {error, enoarray,
                    Var#variable.line, ?E_WARNING, State#state.active_file}),
                throw({error, enoarray})
            end,
            LIdx ++ [Value];
        (Idx, LIdx) ->
            {Value, _Vars} = resolve(Idx, State),
            LIdx ++ [Value]
    end, [], Indexes),
    Var#variable{idx=NewIndexes}.


resolve_txt(Texts, Line, State) ->
    lists:foldr(fun
        (true, {ResultTxt,NS}) ->
            {<<"1",ResultTxt/binary>>,NS};
        (Data, {ResultTxt,NS}) when
                Data =:= undefined orelse
                Data =:= false ->
            {<<ResultTxt/binary>>,NS};
        (Data, {ResultTxt,NS}) when is_binary(Data) ->
            {<<Data/binary,ResultTxt/binary>>,NS};
        (Data, {ResultTxt,NS}) when is_tuple(Data) ->
            {TextRaw,NewState} = resolve(Data, NS),
            Text = ephp_data:to_bin(NS#state.ref, Line, TextRaw),
            {<<Text/binary,ResultTxt/binary>>,NewState}
    end, {<<>>,State}, Texts).


resolve_op(#operation{
    type=Type, expression_left=Op1, expression_right=Op2}, State)
        when Type =:= 'and'; Type =:= 'or'->
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

resolve_op(#operation{type=instanceof, expression_left=Op1,
        expression_right=#constant{name=ClassName}},
        #state{class=Classes}=State) ->
    {OpRes1, State1} = resolve(Op1, State),
    case ephp_class:get(Classes, ClassName) of
        {ok, #class{name=RealClassName}} ->
            {get_class_name(OpRes1) =:= RealClassName, State1};
        {error, enoexist} ->
            {false, State1}
    end;

resolve_op(#operation{type=Type, expression_left=Op1, expression_right=Op2,
                      line=Index},
           #state{active_file=File}=State) ->
    {OpRes1, State1} = resolve(Op1, State),
    {OpRes2, State2} = resolve(Op2, State1),
    {case Type of
        <<"+">> when ?IS_ARRAY(OpRes1) andalso ?IS_ARRAY(OpRes2) ->
            lists:foldl(fun({K,V}, A) ->
                case ephp_array:find(K, A) of
                    error -> ephp_array:store(K, V, A);
                    _ -> A
                end
            end, OpRes1, ephp_array:to_list(OpRes2));
        <<"+">> ->
            ephp_data:zero_if_undef(OpRes1) + ephp_data:zero_if_undef(OpRes2);
        <<"-">> ->
            ephp_data:zero_if_undef(OpRes1) - ephp_data:zero_if_undef(OpRes2);
        <<"*">> ->
            ephp_data:zero_if_undef(OpRes1) * ephp_data:zero_if_undef(OpRes2);
        <<"/">> ->
            A = ephp_data:zero_if_undef(OpRes1),
            B = ephp_data:zero_if_undef(OpRes2),
            if
                B == 0 ->
                    Error = {error, edivzero, Index, ?E_ERROR, File},
                    ephp_error:handle_error(State#state.ref, Error);
                B == infinity ->
                    0;
                true ->
                    A / B
            end;
        <<"%">> ->
            trunc(ephp_data:zero_if_undef(OpRes1)) rem
            trunc(ephp_data:zero_if_undef(OpRes2));
        <<"<">> when OpRes1 =:= undefined -> true;
        <<"<">> when OpRes2 =:= undefined -> false;
        <<"<">> -> OpRes1 < OpRes2;
        <<">">> when OpRes1 =:= undefined -> false;
        <<">">> when OpRes1 =:= undefined -> true;
        <<">">> -> OpRes1 > OpRes2;
        <<">=">> when OpRes1 =:= undefined andalso OpRes2 =:= undefined -> true;
        <<">=">> when OpRes1 =:= undefined -> true;
        <<">=">> when OpRes2 =:= undefined -> false;
        <<">=">> -> OpRes1 >= OpRes2;
        <<"=<">> when OpRes1 =:= undefined andalso OpRes2 =:= undefined -> true;
        <<"=<">> when OpRes1 =:= undefined -> false;
        <<"=<">> when OpRes2 =:= undefined -> true;
        <<"=<">> -> OpRes1 =< OpRes2;
        <<"==">> when
                is_record(OpRes1, reg_instance) andalso
                is_record(OpRes2, reg_instance) ->
            get_class_name(OpRes1) =:= get_class_name(OpRes2);
        <<"==">> -> ephp_data:is_equal(OpRes1, OpRes2);
        <<"===">> -> OpRes1 =:= OpRes2;
        <<"!=">> -> OpRes1 /= OpRes2;
        <<"!==">> -> OpRes1 =/= OpRes2;
        <<"^">> -> ephp_data:zero_if_undef(OpRes1) bxor ephp_data:zero_if_undef(OpRes2);
        <<"|">> -> ephp_data:zero_if_undef(OpRes1) bor ephp_data:zero_if_undef(OpRes2);
        <<"&">> -> ephp_data:zero_if_undef(OpRes1) band ephp_data:zero_if_undef(OpRes2);
        instanceof -> get_class_name(OpRes1) =:= get_class_name(OpRes2)
    end, State2};

resolve_op(Cond, State) ->
    {Value, NewState} = resolve(Cond, State),
    BoolValue = ephp_data:to_bool(Value),
    {BoolValue, NewState}.

get_class_name(#reg_instance{class=#class{name=Name}}) -> Name.

-spec load_state(context()) -> #state{}.

load_state(Context) ->
    erlang:get(Context).

-spec save_state(#state{}) -> #state{} | undefined.

save_state(#state{ref=Ref} = State) ->
    erlang:put(Ref, State).

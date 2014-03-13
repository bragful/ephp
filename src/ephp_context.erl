-module(ephp_context).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile([export_all, warnings_as_errors]).

-include("ephp.hrl").

-record(reg_func, {
    name :: binary(),
    args :: [variable()],
    type :: builtin | php,
    code = [] :: [statement()],
    builtin :: {Module :: atom(), Func :: atom()} | function()
}).

-record(state, {
    vars :: pid(),
    funcs :: pid(),
    timezone = "Europe/Madrid" :: string(),
    output :: pid(),
    const :: pid(),
    global :: pid(),
    destroy = true :: boolean()
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

    get_state/1,

    set_tz/2,
    get_tz/1,

    get_output/1,
    set_output/2,
    set_output_handler/2,
    get_output_handler/1,

    call_func/3,
    register_func/3,
    register_func/4,

    register_const/3,

    set_global/2,
    generate_subcontext/1
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(State) ->
    gen_server:start_link(?MODULE, [State], []).

get(Context, VarPath) ->
    gen_server:call(Context, {get, VarPath}).

set(Context, VarPath, Value) ->
    gen_server:cast(Context, {set, VarPath, Value}).

solve(Context, Expression) ->
    gen_server:call(Context, {resolve, Expression}). 

destroy(Context) ->
    gen_server:cast(Context, stop).

get_state(Context) ->
    gen_server:call(Context, get_state).

register_func(Context, PHPFunc, Module, Fun) when is_atom(Module) and is_atom(Fun) ->  
    gen_server:cast(Context, {register, builtin, PHPFunc, Module, Fun});

register_func(Context, PHPFunc, Args, Code) ->
    gen_server:cast(Context, {register, php, PHPFunc, Args, Code}).

register_func(Context, PHPFunc, Fun) ->
    gen_server:cast(Context, {register, builtin, PHPFunc, Fun}).

register_const(Context, Name, Value) ->
    gen_server:cast(Context, {const, Name, Value}).

call_func(Context, PHPFunc, Args) ->
    gen_server:call(Context, {call, PHPFunc, Args}).

get_tz(Context) ->
    gen_server:call(Context, get_tz).

set_tz(Context, TZ) ->
    gen_server:cast(Context, {set_tz, TZ}).

get_output(Context) ->
    gen_server:call(Context, output).

set_output(Context, Text) ->
    gen_server:cast(Context, {output, Text}).

set_output_handler(Context, Output) ->
    gen_server:cast(Context, {output_handler, Output}).

get_output_handler(Context) ->
    gen_server:call(Context, output_handler).

set_global(Context, GlobalContext) ->
    gen_server:cast(Context, {global, GlobalContext}).

generate_subcontext(Context) ->
    gen_server:call(Context, subcontext).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, Funcs} = ephp_func:start_link(),
    {ok, Output} = ephp_output:start_link(),
    {ok, Const} = ephp_const:start_link(),
    init([#state{
        output = Output,
        funcs = Funcs,
        const = Const}]);

init([#state{}=State]) ->
    {ok, Vars} = ephp_vars:start_link(),
    {ok, State#state{
        vars = Vars
    }};

init([#state{}=State, mirror]) ->
    {ok, State#state{destroy=false}}.

handle_call({get, VarRawPath}, _From, #state{vars=Vars}=State) ->
    Value = ephp_vars:get(Vars, VarRawPath),
    {reply, Value, State};

handle_call({resolve, Expression}, _From, State) ->
    {Value, NewState} = resolve(Expression, State),
    {reply, Value, NewState};

handle_call({call, PHPFunc, Args}, _From, #state{funcs=Funcs}=State) ->
    {reply, case ephp_func:get(Funcs, PHPFunc) of
        error -> throw(eundefun);
        {ok, #reg_func{type=builtin, builtin={Module, Fun}}} ->
            fun(Ctx) -> 
                erlang:apply(Module, Fun, [Ctx|Args]) 
            end;
        {ok, #reg_func{type=builtin, builtin=Fun}} ->
            fun(Ctx) -> 
                erlang:apply(Fun, [Ctx|Args]) 
            end;
        {ok, #reg_func{type=php, code=Code}} ->
            fun(Ctx) ->
                ephp_interpr:process(Ctx, Code)
            end
    end, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(get_tz, _From, #state{timezone=TZ}=State) ->
    {reply, TZ, State};

handle_call(output, _From, #state{output=Output}=State) ->
    {reply, ephp_output:pop(Output), State};

handle_call(subcontext, _From, #state{vars=VarsPID}=State) ->
    {ok, SubContext} = ephp_vars:start_link(),
    {reply, start_link(State#state{
        vars=SubContext,
        global=VarsPID}), State};

handle_call(output_handler, _From, #state{output=Output}=State) ->
    {reply, Output, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({register, php, PHPFunc, Args, Code}, #state{funcs=Funcs}=State) ->
    ephp_func:register_func(Funcs, PHPFunc, Args, Code),
    {noreply, State};

handle_cast({register, builtin, PHPFunc, Fun}, #state{funcs=Funcs}=State) ->
    ephp_func:register_func(Funcs, PHPFunc, Fun),
    {noreply, State};

handle_cast({register, builtin, PHPFunc, Module, Fun}, #state{funcs=Funcs}=State) ->
    ephp_func:register_func(Funcs, PHPFunc, Module, Fun),
    {noreply, State};

handle_cast({const, Name, Value}, #state{const=Const}=State) ->
    ephp_const:set(Const, Name, Value),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({set, VarRawPath, Value}, State) ->
    VarPath = get_var_path(VarRawPath, State),
    change(VarPath, Value, State#state.vars),
    {noreply, State};

handle_cast({set_tz, TZ}, State) ->
    case ezic_zone_map:find(TZ) of
        {zone_not_found,_} -> {noreply, State}; 
        _ -> {noreply, State#state{timezone=TZ}}
    end;

handle_cast({output, Text}, #state{output=Output}=State) ->
    ephp_output:push(Output, Text),
    {noreply, State};

handle_cast({output_handler, Output}, #state{output=OldOutput}=State) ->
    ephp_output:destroy(OldOutput),
    {noreply, State#state{output=Output}};

handle_cast({global, GlobalVarsPID}, State) ->
    {noreply, State#state{global = GlobalVarsPID}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{vars=Vars,destroy=true}) ->
    case is_process_alive(Vars) of
        true -> ephp_vars:destroy(Vars);
        false -> ok  
    end;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

start_mirror(State) ->
    gen_server:start_link(?MODULE, [State, mirror], []).


search(Var, Vars) ->
    ephp_vars:get(Vars, Var).


change(Var, Value, Vars) ->
    ephp_vars:set(Vars, Var, Value).


resolve(true, State) -> 
    {true, State};

resolve(false, State) -> 
    {false, State};

resolve(null, State) -> 
    {undefined, State};

resolve(#assign{variable=Var,expression={ref, RefVar}}, State) ->
    ephp_vars:ref(State#state.vars, Var, State#state.vars, RefVar),
    resolve(RefVar, State);

resolve(#assign{variable=Var,expression=Expr}, State) ->
    VarPath = get_var_path(Var, State),
    {Value, NState} = resolve(Expr, State),
    change(VarPath, Value, NState#state.vars),
    {Value, NState};

resolve(#operation{}=Op, State) ->
    resolve_op(Op, State);

resolve(#int{int=Int}, State) -> 
    {Int, State};

resolve(#float{float=Float}, State) -> 
    {Float, State};

resolve(#text{text=Text}, State) -> 
    {Text, State};

resolve(#text_to_process{text=Texts}, State) ->
    resolve_txt(Texts, State);

resolve({pre_incr, Var}, State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, State#state.vars) of
        undefined -> 
            change(VarPath, 1, State#state.vars),
            {1, State};
        V when is_number(V) -> 
            change(VarPath, V+1, State#state.vars),
            {V+1, State};
        V when is_binary(V) andalso byte_size(V) > 0 ->
            NewVal = try
                list_to_integer(binary_to_list(V)) + 1
            catch error:badarg ->
                ephp_util:increment_code(V)
            end,
            change(VarPath, NewVal, State#state.vars),
            {NewVal, State};
        V -> 
            {V, State}
    end;

resolve({pre_decr, Var}, State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, State#state.vars) of
        undefined -> 
            {undefined, State};
        V when is_number(V) -> 
            change(VarPath, V-1, State#state.vars),
            {V-1, State};
        V -> 
            {V, State}
    end;

resolve({post_incr, Var}, State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, State#state.vars) of
        undefined -> 
            change(VarPath, 1, State#state.vars),
            {undefined, State};
        V when is_number(V) -> 
            change(VarPath, V+1, State#state.vars),
            {V, State};
        V when is_binary(V) andalso byte_size(V) > 0 ->
            NewVal = try
                list_to_integer(binary_to_list(V)) + 1
            catch error:badarg ->
                ephp_util:increment_code(V)
            end,
            change(VarPath, NewVal, State#state.vars),
            {V, State};
        V -> 
            {V, State}
    end;

resolve({post_decr, Var}, State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, State#state.vars) of
        undefined -> 
            {undefined, State};
        V when is_number(V) ->
            change(VarPath, V-1, State#state.vars),
            {V, State};
        V -> 
            {V, State}
    end;

resolve({operation_not, Expr}, State) ->
    EmptyArray = ?DICT:new(),
    case resolve(Expr, State) of
        {false, NewState} -> {true, NewState};
        {<<>>, NewState} -> {true, NewState};
        {0, NewState} -> {true, NewState};
        {<<"0">>, NewState} -> {true, NewState};
        {EmptyArray, NewState} -> {true, NewState};
        {_, NewState} -> {false, NewState}
    end;

resolve({operation_bnot, Expr}, State) ->
    case resolve(Expr, State) of
        {Number, NewState} when is_integer(Number) -> {bnot(Number), NewState};
        {Number, NewState} when is_float(Number) -> {bnot(Number), NewState};
        {Binary, NewState} when is_binary(Binary) -> 
            {<< <<bnot(B)/integer>> || <<B:8/integer>> <= Binary >>, NewState};
        _ -> throw(ebadbnot)
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
    {_I,Array,NState} = lists:foldl(fun
        (#array_element{idx=auto, element=Element}, {I,Dict,NS}) ->
            {Value, NewState} = resolve(Element,NS),
            {I+1,?DICT:store(I,Value,Dict),NewState};
        (#array_element{idx=I, element=Element}, {INum,Dict,NS}) ->
            {Value, NewState} = resolve(Element,NS),
            {Idx, ReNewState} = resolve(I,NewState),
            if
            is_integer(Idx) ->  
                {Idx+1,?DICT:store(Idx,Value,Dict),ReNewState};
            true ->
                {INum,?DICT:store(Idx,Value,Dict),ReNewState}
            end
    end, {0,?DICT:new(),State}, ArrayElements),
    {Array, NState};

resolve({concat, Texts}, State) ->
    resolve_txt(Texts, State);

resolve(#call{name=Fun,args=RawArgs}, #state{vars=Vars,funcs=Funcs}=State) ->
    case ephp_func:get(Funcs, Fun) of
        error -> throw(eundefun);
        {ok,#reg_func{type=builtin, builtin={M,F}}} when is_atom(M) andalso is_atom(F) -> 
            {Args, NState} = lists:foldl(fun(Arg,{Args,S}) ->
                {A,NewState} = resolve(Arg,S),
                {Args ++ [{Arg,A}], NewState}
            end, {[], State}, RawArgs),
            {ok, Mirror} = start_mirror(NState),
            Value = erlang:apply(M,F,[Mirror|Args]),
            destroy(Mirror),
            {Value, NState};
        {ok,#reg_func{type=builtin, builtin=F}} when is_function(F) -> 
            {Args, NState} = lists:foldl(fun(Arg,{Args,S}) ->
                {A,NewState} = resolve(Arg,S),
                {Args ++ [{Arg,A}], NewState}
            end, {[], State}, RawArgs),
            {ok, Mirror} = start_mirror(NState),
            Value = F([Mirror,Args]),
            destroy(Mirror),
            {Value, NState};
        {ok,#reg_func{type=php, args=FuncArgs, code=Code}} ->
            {Args, NState} = lists:foldl(fun(Arg,{Args,S}) ->
                {A,NewState} = resolve(Arg,S),
                {Args ++ [{Arg,A}], NewState}
            end, {[], State}, RawArgs),
            {ok, SubContext} = start_link(NState#state{
                global=Vars}),
            lists:foldl(fun
                ({ref,VarRef}, [{VarName,_}|RestArgs]) ->
                    ephp_vars:ref(NewVars, VarRef, Vars, VarName),
                    RestArgs;
                (FuncArg, [{_,ArgVal}|RestArgs]) ->
                    ephp_vars:set(NewVars, FuncArg, ArgVal),
                    RestArgs;
                (_FuncArg, []) ->
                    []
            end, Args, FuncArgs),
            Value = case ephp_interpr:run(SubContext, #eval{statements=Code}) of
                {return, V} -> solve(SubContext, V);
                _ -> null
            end,
            destroy(SubContext),
            {Value, State}
    end;

resolve({global, _Var}, #state{global=undefined}=State) ->
    {null, State};

resolve({global, GlobalVar}, #state{
        global=GlobalVars,
        vars=Vars}=State) ->
    ephp_vars:ref(Vars, GlobalVar, GlobalVars, GlobalVar),
    {null, State};

resolve(#constant{name=Name}, #state{const=Const}=State) ->
    {ephp_const:get(Const, Name), State};

resolve(Unknown, #state{}=State) ->
    %% TODO: better handle of this errors
    io:format("~p - ~p~n", [Unknown,State]),
    throw(eundeftoken).


resolve_var(#variable{idx=[]}=Var, State) ->
    {search(Var, State#state.vars), State};

resolve_var(#variable{idx=Indexes}=Var, State) ->
    {NewIndexes, NewState} = lists:foldl(fun(Idx,{I,NS}) ->
        {Value, NState} = resolve(Idx, NS),
        {I ++ [Value], NState}
    end, {[],State}, Indexes),
    Value = search(Var#variable{idx=NewIndexes}, NewState#state.vars),
    {Value, NewState}.


get_var_path(#variable{idx=[]}=Var, _State) ->
    Var;

get_var_path(#variable{idx=Indexes}=Var, State) ->
    NewIndexes = lists:map(fun(Idx) ->
        {Value, _Vars} = resolve(Idx, State),
        Value
    end, Indexes),
    Var#variable{idx=NewIndexes}.


resolve_txt(Texts, State) ->
    lists:foldr(fun
        (Data, {ResultTxt,NS}) when is_binary(Data) ->
            {<<Data/binary,ResultTxt/binary>>,NS};
        (Data, {ResultTxt,NS}) when is_tuple(Data) ->
            {TextRaw,NewState} = resolve(Data, NS),
            Text = ephp_util:to_bin(TextRaw),
            {<<Text/binary,ResultTxt/binary>>,NewState}
    end, {<<>>,State}, Texts).


resolve_op(#operation{type=Type, expression_left=Op1, expression_right=Op2}, State) ->
    {OpRes1, State1} = resolve(Op1, State),
    {OpRes2, State2} = resolve(Op2, State1),
    {case Type of
        <<"+">> -> ephp_util:zero_if_undef(OpRes1) + ephp_util:zero_if_undef(OpRes2);
        <<"-">> -> ephp_util:zero_if_undef(OpRes1) - ephp_util:zero_if_undef(OpRes2);
        <<"*">> -> ephp_util:zero_if_undef(OpRes1) * ephp_util:zero_if_undef(OpRes2);
        <<"/">> -> 
            A = ephp_util:zero_if_undef(OpRes1),
            B = ephp_util:zero_if_undef(OpRes2),
            if 
                B == 0 -> throw(edivzero);
                true -> A / B
            end;
        <<"<">> -> OpRes1 < OpRes2;
        <<">">> -> OpRes1 > OpRes2;
        <<">=">> -> OpRes1 >= OpRes2;
        <<"=<">> -> OpRes1 =< OpRes2;
        <<"==">> -> OpRes1 == OpRes2;
        <<"===">> -> OpRes1 =:= OpRes2;
        <<"!=">> -> OpRes1 /= OpRes2;
        <<"!==">> -> OpRes1 =/= OpRes2;
        'and' -> OpRes1 andalso OpRes2;
        'or' -> OpRes1 orelse OpRes2;
        <<"^">> -> OpRes1 bxor OpRes2;
        <<"|">> -> OpRes1 bor OpRes2;
        <<"&">> -> OpRes1 band OpRes2
    end, State2}.

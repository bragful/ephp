-module(ephp_context).

-compile([warnings_as_errors]).

-include("ephp.hrl").

-record(reg_func, {
    name :: binary(),
    args :: [variable()],
    type :: builtin | php,
    code = [] :: [statement()],
    builtin :: {Module :: atom(), Func :: atom()} | function()
}).

-record(state, {
    ref :: reference(),
    vars :: pid(),
    funcs :: pid(),
    timezone = "Europe/Madrid" :: string(),
    output :: pid(),
    const :: pid(),
    global :: pid(),
    include :: pid()
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

    get_const/2,
    register_const/3,

    load/2,
    load_once/2,

    set_global/2,
    generate_subcontext/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    {ok, Funcs} = ephp_func:start_link(),
    {ok, Output} = ephp_output:start_link(),
    {ok, Const} = ephp_const:start_link(),
    {ok, Inc} = ephp_include:start_link(),
    start_link(#state{
        ref = Ref,
        output = Output,
        funcs = Funcs,
        const = Const,
        include = Inc
    }).

start_link(#state{ref=Ref}=State) ->
    {ok, Vars} = ephp_vars:start_link(),
    erlang:put(Ref, State#state{
        vars = Vars
    }),
    {ok, Ref}.

start_mirror(#state{}=State) ->
    Ref = make_ref(),
    erlang:put(Ref, State#state{ref=Ref}),
    {ok, Ref}.

get(Context, VarPath) ->
    #state{vars=Vars} = erlang:get(Context),
    ephp_vars:get(Vars, VarPath).

set(Context, VarPath, Value) ->
    State = erlang:get(Context),
    ephp_vars:set(State#state.vars, get_var_path(VarPath, State), Value),
    ok.

solve(Context, Expression) ->
    State = erlang:get(Context),
    {Value, NewState} = resolve(Expression, State),
    erlang:put(Context, NewState),
    Value.

destroy(Context) ->
    erlang:erase(Context).

get_state(Context) ->
    get(Context).

register_func(Context, PHPFunc, Module, Fun) when is_atom(Module) and is_atom(Fun) ->  
    #state{funcs=Funcs} = erlang:get(Context),
    ephp_func:register_func(Funcs, PHPFunc, Module, Fun),
    ok;

register_func(Context, PHPFunc, Args, Code) ->
    #state{funcs=Funcs} = erlang:get(Context),
    ephp_func:register_func(Funcs, PHPFunc, Args, Code),
    ok.

register_func(Context, PHPFunc, Fun) ->
    #state{funcs=Funcs} = erlang:get(Context),
    ephp_func:register_func(Funcs, PHPFunc, Fun),
    ok.

get_const(Context, Name) ->
    #state{const=Const} = erlang:get(Context),
    ephp_const:get(Const, Name).

register_const(Context, Name, Value) ->
    #state{const=Const} = erlang:get(Context),
    ephp_const:set(Const, Name, Value),
    ok.

call_func(Context, PHPFunc, Args) ->
    #state{funcs=Funcs, const=Const} = erlang:get(Context),
    case ephp_func:get(Funcs, PHPFunc) of
    error ->
        %% TODO: enhance the error handling!
        io:format("~p~n", [{PHPFunc, Args}]),
        throw(eundefun);
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
            OldFunc = ephp_const:get(Const, <<"__FUNCTION__">>),
            ephp_const:set(Const, <<"__FUNCTION__">>, PHPFunc), 
            Res = ephp_interpr:process(Ctx, Code),
            ephp_const:set(Const, <<"__FUNCTION__">>, OldFunc),
            Res
        end
    end.

get_tz(Context) ->
    #state{timezone=TZ} = erlang:get(Context),
    TZ.

set_tz(Context, TZ) ->
    State = erlang:get(Context),
    erlang:put(Context, State#state{timezone=TZ}),
    ok.

get_output(Context) ->
    #state{output=Output} = erlang:get(Context),
    ephp_output:pop(Output).

set_output(Context, Text) ->
    #state{output=Output} = erlang:get(Context),
    ephp_output:push(Output, Text),
    ok.

set_output_handler(Context, Output) ->
    State = erlang:get(Context),
    ephp_output:destroy(State#state.output),
    erlang:put(Context, State#state{output=Output}),
    ok.

get_output_handler(Context) ->
    #state{output=Output} = erlang:get(Context),
    Output.

load(Context, File) ->
    #state{include=Inc} = erlang:get(Context),
    ephp_include:load(Inc, File).

load_once(Context, File) ->
    #state{include=Inc} = erlang:get(Context),
    ephp_include:load_once(Inc, File).

set_global(Context, GlobalContext) ->
    State = erlang:get(Context),
    erlang:put(Context, State#state{global=GlobalContext}),
    ok.

generate_subcontext(Context) ->
    #state{vars=VarsPID} = State = erlang:get(Context),
    {ok, SubContext} = ephp_vars:start_link(),
    start_link(State#state{
        vars=SubContext,
        global=VarsPID}).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

resolve(true, State) -> 
    {true, State};

resolve(false, State) -> 
    {false, State};

resolve(null, State) -> 
    {undefined, State};

resolve(#assign{variable=Var,expression=#ref{var=RefVar}}, State) ->
    ephp_vars:ref(State#state.vars, Var, State#state.vars, RefVar),
    resolve(RefVar, State);

resolve(#assign{variable=Var,expression=Expr}, State) ->
    VarPath = get_var_path(Var, State),
    {Value, NState} = resolve(Expr, State),
    ephp_vars:set(NState#state.vars, VarPath, Value),
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

resolve({pre_incr, Var, _Line}, State) ->
    VarPath = get_var_path(Var, State),
    case ephp_vars:get(State#state.vars, VarPath) of
        undefined -> 
            ephp_vars:set(State#state.vars, VarPath, 1),
            {1, State};
        V when is_number(V) -> 
            ephp_vars:set(State#state.vars, VarPath, V+1),
            {V+1, State};
        V when is_binary(V) andalso byte_size(V) > 0 ->
            NewVal = try
                list_to_integer(binary_to_list(V)) + 1
            catch error:badarg ->
                ephp_util:increment_code(V)
            end,
            ephp_vars:set(State#state.vars, VarPath, NewVal),
            {NewVal, State};
        V -> 
            {V, State}
    end;

resolve({pre_decr, Var, _Line}, State) ->
    VarPath = get_var_path(Var, State),
    case ephp_vars:get(State#state.vars, VarPath) of
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
    case ephp_vars:get(State#state.vars, VarPath) of
        undefined -> 
            ephp_vars:set(State#state.vars, VarPath, 1),
            {undefined, State};
        V when is_number(V) -> 
            ephp_vars:set(State#state.vars, VarPath, V+1),
            {V, State};
        V when is_binary(V) andalso byte_size(V) > 0 ->
            NewVal = try
                list_to_integer(binary_to_list(V)) + 1
            catch error:badarg ->
                ephp_util:increment_code(V)
            end,
            ephp_vars:set(State#state.vars, VarPath, NewVal),
            {V, State};
        V -> 
            {V, State}
    end;

resolve({post_decr, Var, _Line}, State) ->
    VarPath = get_var_path(Var, State),
    case ephp_vars:get(State#state.vars, VarPath) of
        undefined -> 
            {undefined, State};
        V when is_number(V) ->
            ephp_vars:set(State#state.vars, VarPath, V-1),
            {V, State};
        V -> 
            {V, State}
    end;

resolve({operation_not, Expr, _Line}, State) ->
    EmptyArray = ?DICT:new(),
    case resolve(Expr, State) of
        {false, NewState} -> {true, NewState};
        {<<>>, NewState} -> {true, NewState};
        {0, NewState} -> {true, NewState};
        {<<"0">>, NewState} -> {true, NewState};
        {EmptyArray, NewState} -> {true, NewState};
        {_, NewState} -> {false, NewState}
    end;

resolve({operation_bnot, Expr, _Line}, State) ->
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

resolve(#concat{texts=Texts}, State) ->
    resolve_txt(Texts, State);

resolve(#call{name=Fun}=Call, State) when not is_binary(Fun) ->
    {Name, NewState} = resolve(Fun, State),
    resolve(Call#call{name=Name}, NewState);

resolve(#call{name=Fun,args=RawArgs,line=Index}, #state{vars=Vars,funcs=Funcs,const=Const}=State) ->
    case ephp_func:get(Funcs, Fun) of
        error ->
            throw({error, eundefun, ephp_util:get_line(Index), Fun});
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
            Value = F([Mirror|Args]),
            destroy(Mirror),
            {Value, NState};
        {ok,#reg_func{type=php, args=FuncArgs, code=Code}} ->
            {Args, NState} = lists:foldl(fun(Arg,{Args,S}) ->
                {A,NewState} = resolve(Arg,S),
                {Args ++ [{Arg,A}], NewState}
            end, {[], State}, RawArgs),
            {ok, NewVars} = ephp_vars:start_link(),
            {ok, SubContext} = start_mirror(NState#state{
                vars=NewVars,
                global=Vars}),
            lists:foldl(fun
                (#ref{var=VarRef}, [{VarName,_}|RestArgs]) ->
                    ephp_vars:ref(NewVars, VarRef, Vars, VarName),
                    RestArgs;
                (FuncArg, [{_,ArgVal}|RestArgs]) ->
                    ephp_vars:set(NewVars, FuncArg, ArgVal),
                    RestArgs;
                (_FuncArg, []) ->
                    []
            end, Args, FuncArgs),
            OldFunc = ephp_const:get(Const, <<"__FUNCTION__">>),
            ephp_const:set(Const, <<"__FUNCTION__">>, Fun),
            Value = case ephp_interpr:run(SubContext, #eval{statements=Code}) of
                {return, V} -> V;
                _ -> null
            end,
            destroy(SubContext),
            ephp_vars:destroy(NewVars), 
            ephp_const:set(Const, <<"__FUNCTION__">>, OldFunc), 
            {Value, State}
    end;

resolve({global, _Var,_Line}, #state{global=undefined}=State) ->
    {null, State};

resolve({global, GVars,_Line}, #state{
        global=GlobalVars,
        vars=Vars}=State) ->
    lists:foreach(fun(GlobalVar) ->
        ephp_vars:ref(Vars, GlobalVar, GlobalVars, GlobalVar)
    end, GVars),
    {null, State};

resolve(#constant{name=Name}, #state{const=Const}=State) ->
    {ephp_const:get(Const, Name), State};

resolve(#print_text{text=Text}, #state{output=Output}=State) ->
    ephp_output:push(Output, Text),
    {null, State};

resolve(undefined, State) ->
    {null, State};

resolve(auto, _State) ->
    % Fatal error: Cannot use [] for reading
    throw(earrayundef);

resolve(Unknown, #state{}=State) ->
    %% TODO: better handle of this errors
    io:format("~p - ~p~n", [Unknown,State]),
    throw(eundeftoken).


resolve_var(#variable{idx=[]}=Var, State) ->
    {ephp_vars:get(State#state.vars, Var), State};

resolve_var(#variable{idx=Indexes}=Var, State) ->
    {NewIndexes, NewState} = lists:foldl(fun(Idx,{I,NS}) ->
        {Value, NState} = resolve(Idx, NS),
        {I ++ [Value], NState}
    end, {[],State}, Indexes),
    Value = ephp_vars:get(NewState#state.vars, Var#variable{idx=NewIndexes}),
    {Value, NewState}.


get_var_path(#variable{idx=[]}=Var, _State) ->
    Var;

get_var_path(#variable{idx=Indexes}=Var, #state{vars=Vars}=State) ->
    NewIndexes = lists:foldl(fun
        (auto, LIdx) ->
            NewEntry = Var#variable{idx=LIdx},
            Value = case ephp_vars:get(Vars, NewEntry) of
            undefined -> 
                0;
            Array -> 
                ?DICT:fold(fun
                    (K,_V,Max) when is_integer(K) andalso K >= Max -> K+1;
                    (_K,_V,Max) -> Max
                end, 0, Array)
            end,
            LIdx ++ [Value];
        (Idx, LIdx) ->
            {Value, _Vars} = resolve(Idx, State),
            LIdx ++ [Value]
    end, [], Indexes),
    Var#variable{idx=NewIndexes}.


resolve_txt(Texts, State) ->
    lists:foldr(fun
        (true, {ResultTxt,NS}) ->
            {<<"1",ResultTxt/binary>>,NS};
        (Data, {ResultTxt,NS}) when 
                Data =:= null orelse 
                Data =:= false ->
            {<<ResultTxt/binary>>,NS};
        (Data, {ResultTxt,NS}) when is_binary(Data) ->
            {<<Data/binary,ResultTxt/binary>>,NS};
        (Data, {ResultTxt,NS}) when is_tuple(Data) ->
            {TextRaw,NewState} = resolve(Data, NS),
            Text = ephp_util:to_bin(TextRaw),
            {<<Text/binary,ResultTxt/binary>>,NewState}
    end, {<<>>,State}, Texts).


resolve_op(#operation{
    type=Type, expression_left=Op1, expression_right=Op2}, State) 
        when Type =:= 'and'; Type =:= 'or'->
    {RawOpRes1, State1} = resolve(Op1, State),
    OpRes1 = ephp_util:to_bool(RawOpRes1),
    case Type of
        'and' when OpRes1 =:= false -> 
            {false, State1};
        'and' ->
            {OpRes2, State2} = resolve(Op2, State1),
            {ephp_util:to_bool(OpRes2), State2};
        'or' when OpRes1 =:= true -> 
            {true, State1};
        'or' ->
            {OpRes2, State2} = resolve(Op2, State1),
            {ephp_util:to_bool(OpRes2), State2}
    end;

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
        <<"%">> -> 
            trunc(ephp_util:zero_if_undef(OpRes1)) rem 
            trunc(ephp_util:zero_if_undef(OpRes2));
        <<"<">> -> OpRes1 < OpRes2;
        <<">">> -> OpRes1 > OpRes2;
        <<">=">> -> OpRes1 >= OpRes2;
        <<"=<">> -> OpRes1 =< OpRes2;
        <<"==">> -> OpRes1 == OpRes2;
        <<"===">> -> OpRes1 =:= OpRes2;
        <<"!=">> -> OpRes1 /= OpRes2;
        <<"!==">> -> OpRes1 =/= OpRes2;
        <<"^">> -> OpRes1 bxor OpRes2;
        <<"|">> -> OpRes1 bor OpRes2;
        <<"&">> -> OpRes1 band OpRes2
    end, State2}.

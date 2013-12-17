-module(ephp_context).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile([export_all, warnings_as_errors]).

-include("ephp.hrl").

-record(state, {
    vars = dict:new() :: dict(),
    funcs = dict:new() :: dict(),
    timezone = "Europe/Madrid" :: string()
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

    call_func/3,
    register_func/4
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

register_func(Context, PHPFunc, Module, Fun) ->
    gen_server:cast(Context, {register, PHPFunc, Module, Fun}).

call_func(Context, PHPFunc, Args) ->
    gen_server:call(Context, {call, PHPFunc, Args}).

get_tz(Context) ->
    gen_server:call(Context, get_tz).

set_tz(Context, TZ) ->
    gen_server:cast(Context, {set_tz, TZ}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}};

init([#state{}=State]) ->
    {ok, State}.

handle_call({get, VarRawPath}, _From, State) ->
    {reply, resolve_var(VarRawPath, State), State};

handle_call({resolve, Expression}, _From, State) ->
    {Value, NewVars} = resolve(Expression,State),
    {reply, Value, State#state{vars = NewVars}};

handle_call({call, PHPFunc, Args}, _From, #state{funcs=Funcs}=State) ->
    {reply, case ?DICT:find(PHPFunc, Funcs) of
        error -> throw(eundefun);
        {ok, {Module, Fun}} -> {Module, Fun, Args}
    end, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(get_tz, _From, #state{timezone=TZ}=State) ->
    {reply, TZ, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({register, PHPFunc, Module, Fun}, #state{funcs=Funcs}=State) ->
    {noreply, State#state{funcs=?DICT:store(PHPFunc, {Module, Fun}, Funcs)}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({set, VarRawPath, Value}, #state{vars=Vars}=State) ->
    VarPath = get_var_path(VarRawPath, State),
    NewVars = change(VarPath, Value, Vars),
    {noreply, State#state{vars = NewVars}};

handle_cast({set_tz, TZ}, State) ->
    case ezic_zone_map:find(TZ) of
        {zone_not_found,_} -> {noreply, State}; 
        _ -> {noreply, State#state{timezone=TZ}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

search(#variable{name=Root, idx=[]}, Vars) ->
    case ?DICT:find(Root, Vars) of
        error -> undefined;
        {ok, Value} -> Value
    end;

search(#variable{name=Root, idx=[NewRoot|Idx]}, Vars) ->
    case ?DICT:find(Root, Vars) of
        {ok, NewVars} when ?IS_DICT(NewVars) -> 
            search(#variable{name=NewRoot, idx=Idx}, NewVars);
        _ -> 
            undefined
    end.


change(#variable{name=Root, idx=[]}, Value, Vars) ->
    ?DICT:store(Root, Value, Vars);

change(#variable{name=Root, idx=[NewRoot|Idx]}, Value, Vars) ->
    case ?DICT:find(Root, Vars) of
    {ok, NewVars} when ?IS_DICT(NewVars) -> 
        ?DICT:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value, NewVars), Vars);
    _ -> 
        ?DICT:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value, ?DICT:new()), Vars)
    end.


resolve(true, #state{vars=Vars}) -> 
    {true, Vars};

resolve(false, #state{vars=Vars}) -> 
    {false, Vars};

resolve(null, #state{vars=Vars}) -> 
    {undefined, Vars};

resolve(#assign{variable=Var,expression=Expr}, State) ->
    VarPath = get_var_path(Var, State),
    {Value, NewVars} = resolve(Expr, State),
    {Value, change(VarPath, Value, NewVars)};

resolve(#operation{}=Op, #state{vars=Vars}=State) ->
    {resolve_op(Op, State), Vars};

resolve(#int{int=Int}, #state{vars=Vars}) -> 
    {Int, Vars};

resolve(#float{float=Float}, #state{vars=Vars}) -> 
    {Float, Vars};

resolve(#text{text=Text}, #state{vars=Vars}) -> 
    {Text, Vars};

resolve(#text_to_process{text=Texts}, State) ->
    resolve_txt(Texts, State);

resolve({pre_incr, Var}, #state{vars=Vars}=State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, Vars) of
        undefined -> 
            {1, change(VarPath, 1, Vars)};
        V when is_number(V) -> 
            {V+1, change(VarPath, V+1, Vars)};
        V when is_binary(V) andalso byte_size(V) > 0 ->
            NewVal = try
                list_to_integer(binary_to_list(V)) + 1
            catch error:badarg ->
                ephp_util:increment_code(V)
            end,
            {NewVal, change(VarPath, NewVal, Vars)};
        V -> 
            {V, Vars}
    end;

resolve({pre_decr, Var}, #state{vars=Vars}=State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, Vars) of
        undefined -> 
            {undefined, Vars};
        V when is_number(V) -> 
            {V-1, change(VarPath, V-1, Vars)};
        V -> 
            {V, Vars}
    end;

resolve({post_incr, Var}, #state{vars=Vars}=State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, Vars) of
        undefined -> 
            {undefined, change(VarPath, 1, Vars)};
        V when is_number(V) -> 
            {V, change(VarPath, V+1, Vars)};
        V when is_binary(V) andalso byte_size(V) > 0 ->
            NewVal = try
                list_to_integer(binary_to_list(V)) + 1
            catch error:badarg ->
                ephp_util:increment_code(V)
            end,
            {V, change(VarPath, NewVal, Vars)};
        V -> 
            {V, Vars}
    end;

resolve({post_decr, Var}, #state{vars=Vars}=State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, Vars) of
        undefined -> 
            {undefined, Vars};
        V when is_number(V) -> 
            {V, change(VarPath, V-1, Vars)};
        V -> 
            {V, Vars}
    end;

resolve(#if_block{conditions=Cond}=IfBlock, State) ->
    case resolve_op(Cond, State) of
    true ->
        resolve(IfBlock#if_block.true_block, State);
    false ->
        resolve(IfBlock#if_block.false_block, State)
    end;

resolve(#variable{}=Var, #state{vars=Vars}=State) ->
    {resolve_var(Var, State), Vars};

resolve(#array{elements=ArrayElements}, #state{vars=Vars}=State) ->
    {_I,Array,NVars} = lists:foldl(fun
        (#array_element{idx=auto, element=Element}, {I,Dict,V}) ->
            {Value, NewVars} = resolve(Element,State#state{vars=V}),
            {I+1,?DICT:store(I,Value,Dict),NewVars};
        (#array_element{idx=I, element=Element}, {INum,Dict,V}) ->
            {Value, NewVars} = resolve(Element,State#state{vars=V}),
            {Idx, ReNewVars} = resolve(I,State#state{vars=NewVars}),
            if
            is_integer(Idx) ->  
                {Idx+1,?DICT:store(Idx,Value,Dict),ReNewVars};
            true ->
                {INum,?DICT:store(Idx,Value,Dict),ReNewVars}
            end
    end, {0,?DICT:new(),Vars}, ArrayElements),
    {Array, NVars};

resolve({concat, Texts}, State) ->
    resolve_txt(Texts, State);

resolve(#call{name=Fun,args=RawArgs}, #state{vars=Vars,funcs=Funcs}=State) ->
    case ?DICT:find(Fun, Funcs) of
        error -> throw(eundefun);
        {ok,{M,F}} -> 
            {Args, NVars} = lists:foldl(fun(Arg,{Args,V}) ->
                {A,NV} = resolve(Arg,State#state{vars=V}),
                {Args ++ [A], NV}
            end, {[], Vars}, RawArgs),
            {ok, Mirror} = start_link(State#state{vars=NVars}),
            Value = erlang:apply(M,F,[Mirror|Args]),
            MirrorState = get_state(Mirror),
            destroy(Mirror),
            {Value, MirrorState#state.vars}
    end;

resolve(_Unknown, _State) ->
    throw(eundeftoken).


resolve_var(#variable{idx=[]}=Var, #state{vars=Vars}) ->
    search(Var, Vars);

resolve_var(#variable{idx=Indexes}=Var, #state{vars=Vars}=State) ->
    %% TODO: resolve index could be modified the value if it's used ++ or --
    NewIndexes = lists:map(fun(Idx) ->
        {Value, _Vars} = resolve(Idx, State),
        Value
    end, Indexes),
    search(Var#variable{idx=NewIndexes}, Vars).


get_var_path(#variable{idx=[]}=Var, _State) ->
    Var;

get_var_path(#variable{idx=Indexes}=Var, State) ->
    NewIndexes = lists:map(fun(Idx) ->
        {Value, _Vars} = resolve(Idx, State),
        Value
    end, Indexes),
    Var#variable{idx=NewIndexes}.


resolve_txt(Texts, #state{vars=Variables}=State) ->
    lists:foldr(fun
        (Data, {ResultTxt,Vars}) when is_binary(Data) ->
            {<<Data/binary,ResultTxt/binary>>,Vars};
        (Data, {ResultTxt,_Vars}) when is_tuple(Data) ->
            {TextRaw,NewVars} = resolve(Data, State),
            Text = ephp_util:to_bin(TextRaw),
            {<<Text/binary, ResultTxt/binary>>,NewVars}
    end, {<<>>,Variables}, Texts).


resolve_op(#operation{type=Type, expression_left=Op1, expression_right=Op2}, State) ->
    {OpRes1, _Vars} = resolve(Op1, State),
    {OpRes2, _Vars} = resolve(Op2, State),
    case Type of
        <<"+">> -> ephp_util:zero_if_undef(OpRes1) + ephp_util:zero_if_undef(OpRes2);
        <<"-">> -> ephp_util:zero_if_undef(OpRes1) - ephp_util:zero_if_undef(OpRes2);
        <<"*">> -> ephp_util:zero_if_undef(OpRes1) * ephp_util:zero_if_undef(OpRes2);
        <<"/">> -> ephp_util:zero_if_undef(OpRes1) / ephp_util:zero_if_undef(OpRes2);
        <<"<">> -> OpRes1 < OpRes2;
        <<">">> -> OpRes1 > OpRes2;
        <<">=">> -> OpRes1 >= OpRes2;
        <<"=<">> -> OpRes1 =< OpRes2;
        <<"==">> -> OpRes1 == OpRes2;
        <<"===">> -> OpRes1 =:= OpRes2;
        <<"!=">> -> OpRes1 /= OpRes2;
        <<"!==">> -> OpRes1 =/= OpRes2;
        'and' -> OpRes1 andalso OpRes2;
        'or' -> OpRes1 orelse OpRes2
    end.

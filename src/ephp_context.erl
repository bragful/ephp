-module(ephp_context).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile([export_all, warnings_as_errors]).

-include("ephp.hrl").

-record(state, {
    vars = dict:new(),
    funcs = dict:new()
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

get(Context, VarPath) ->
    gen_server:call(Context, {get, VarPath}).

set(Context, VarPath, Value) ->
    gen_server:cast(Context, {set, VarPath, Value}).

solve(Context, Expression) ->
    gen_server:call(Context, {resolve, Expression}). 

destroy(Context) ->
    gen_server:cast(Context, stop).

register_func(Context, PHPFunc, Module, Fun) ->
    gen_server:cast(Context, {register, PHPFunc, Module, Fun}).

call_func(Context, PHPFunc, Args) ->
    gen_server:call(Context, {call, PHPFunc, Args}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call({get, VarRawPath}, _From, #state{vars=Vars,funcs=Funcs}=State) ->
    {reply, resolve_var(VarRawPath, Vars, Funcs), State};

handle_call({resolve, Expression}, _From, #state{vars=Vars,funcs=Funcs}=State) ->
    {Value, NewVars} = resolve(Expression,Vars,Funcs),
    {reply, Value, State#state{vars = NewVars}};

handle_call({call, PHPFunc, Args}, _From, #state{funcs=Funcs}=State) ->
    {reply, case dict:find(PHPFunc, Funcs) of
        error -> throw(eundefun);
        {ok, {Module, Fun}} -> {Module, Fun, Args}
    end, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({register, PHPFunc, Module, Fun}, #state{funcs=Funcs}=State) ->
    {noreply, State#state{funcs=dict:store(PHPFunc, {Module, Fun}, Funcs)}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({set, VarRawPath, Value}, #state{vars=Vars,funcs=Funcs}=State) ->
    VarPath = get_var_path(VarRawPath, Vars, Funcs),
    NewVars = change(VarPath, Value, Vars),
    {noreply, State#state{vars = NewVars}};

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
    case dict:find(Root, Vars) of
        error -> undefined;
        {ok, Value} -> Value
    end;

search(#variable{name=Root, idx=[NewRoot|Idx]}, Vars) ->
    case dict:find(Root, Vars) of
        {ok, NewVars} when ?IS_DICT(NewVars) -> 
            search(#variable{name=NewRoot, idx=Idx}, NewVars);
        _ -> 
            undefined
    end.


change(#variable{name=Root, idx=[]}, Value, Vars) ->
    dict:store(Root, Value, Vars);

change(#variable{name=Root, idx=[NewRoot|Idx]}, Value, Vars) ->
    case dict:find(Root, Vars) of
    {ok, NewVars} when ?IS_DICT(NewVars) -> 
        dict:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value, NewVars), Vars);
    _ -> 
        dict:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value, dict:new()), Vars)
    end.


resolve(true, Vars, _Funcs) -> 
    {true, Vars};

resolve(false, Vars, _Funcs) -> 
    {false, Vars};

resolve(null, Vars, _Funcs) -> 
    {undefined, Vars};

resolve(#assign{variable=Var,expression=Expr}, Vars, Funcs) ->
    VarPath = get_var_path(Var, Vars, Funcs),
    {Value, NewVars} = resolve(Expr, Vars, Funcs),
    {Value, change(VarPath, Value, NewVars)};

resolve(#operation{}=Op, Vars, Funcs) ->
    {resolve_op(Op, Vars, Funcs), Vars};

resolve(#int{int=Int}, Vars, _Funcs) -> 
    {Int, Vars};

resolve(#float{float=Float}, Vars, _Funcs) -> 
    {Float, Vars};

resolve(#text{text=Text}, Vars, _Funcs) -> 
    {Text, Vars};

resolve(#text_to_process{text=Texts}, Vars, Funcs) ->
    resolve_txt(Texts, Vars, Funcs);

resolve({pre_incr, Var}, Vars, Funcs) ->
    VarPath = get_var_path(Var, Vars, Funcs),
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

resolve({pre_decr, Var}, Vars, Funcs) ->
    VarPath = get_var_path(Var, Vars, Funcs),
    case search(VarPath, Vars) of
        undefined -> 
            {undefined, Vars};
        V when is_number(V) -> 
            {V-1, change(VarPath, V-1, Vars)};
        V -> 
            {V, Vars}
    end;

resolve({post_incr, Var}, Vars, Funcs) ->
    VarPath = get_var_path(Var, Vars, Funcs),
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

resolve({post_decr, Var}, Vars, Funcs) ->
    VarPath = get_var_path(Var, Vars, Funcs),
    case search(VarPath, Vars) of
        undefined -> 
            {undefined, Vars};
        V when is_number(V) -> 
            {V, change(VarPath, V-1, Vars)};
        V -> 
            {V, Vars}
    end;

resolve(#if_block{conditions=Cond}=IfBlock, Vars, Funcs) ->
    case resolve_op(Cond, Vars, Funcs) of
    true ->
        resolve(IfBlock#if_block.true_block, Vars, Funcs);
    false ->
        resolve(IfBlock#if_block.false_block, Vars, Funcs)
    end;

resolve(#variable{}=Var, Vars, Funcs) ->
    {resolve_var(Var, Vars, Funcs), Vars};

resolve(#array{elements=ArrayElements}, Vars, Funcs) ->
    {_I,Array,NVars} = lists:foldl(fun
        (#array_element{idx=auto, element=Element}, {I,Dict,V}) ->
            {Value, NewVars} = resolve(Element,V,Funcs),
            {I+1,dict:store(I,Value,Dict),NewVars};
        (#array_element{idx=I, element=Element}, {INum,Dict,V}) ->
            {Value, NewVars} = resolve(Element,V,Funcs),
            {Idx, ReNewVars} = resolve(I,NewVars,Funcs),
            if
            is_integer(Idx) ->  
                {Idx+1,dict:store(Idx,Value,Dict),ReNewVars};
            true ->
                {INum,dict:store(Idx,Value,Dict),ReNewVars}
            end
    end, {0,dict:new(),Vars}, ArrayElements),
    {Array, NVars};

resolve({concat, Texts}, Vars, Funcs) ->
    resolve_txt(Texts, Vars, Funcs);

resolve(#call{name=Fun,args=RawArgs}, Vars, Funcs) ->
    case dict:find(Fun, Funcs) of
        error -> throw(eundefun);
        {ok,{M,F}} -> 
            {Args, NVars} = lists:foldl(fun(Arg,{Args,V}) ->
                {A,NV} = resolve(Arg,V,Funcs),
                {Args ++ [A], NV}
            end, {[], Vars}, RawArgs),
            {erlang:apply(M,F,Args), NVars}
    end;

resolve(_Unknown, _Vars, _Funcs) ->
    throw(eundeftoken).


resolve_var(#variable{idx=[]}=Var, Vars, _Funcs) ->
    search(Var, Vars);

resolve_var(#variable{idx=Indexes}=Var, Vars, Funcs) ->
    NewIndexes = lists:map(fun(Idx) ->
        {Value, _Vars} = resolve(Idx, Vars, Funcs),
        Value
    end, Indexes),
    search(Var#variable{idx=NewIndexes}, Vars).


get_var_path(#variable{idx=[]}=Var, _Vars, _Funcs) ->
    Var;

get_var_path(#variable{idx=Indexes}=Var, Vars, Funcs) ->
    NewIndexes = lists:map(fun(Idx) ->
        {Value, _Vars} = resolve(Idx, Vars, Funcs),
        Value
    end, Indexes),
    Var#variable{idx=NewIndexes}.


resolve_txt(Texts, Variables, Funcs) ->
    lists:foldr(fun
        (Data, {ResultTxt,Vars}) when is_binary(Data) ->
            {<<Data/binary,ResultTxt/binary>>,Vars};
        (Data, {ResultTxt,Vars}) when is_tuple(Data) ->
            {TextRaw,NewVars} = resolve(Data, Vars, Funcs),
            Text = ephp_util:to_bin(TextRaw),
            {<<Text/binary, ResultTxt/binary>>,NewVars}
    end, {<<>>,Variables}, Texts).


resolve_op(#operation{type=Type, expression_left=Op1, expression_right=Op2}, Vars, Funcs) ->
    {OpRes1, _Vars} = resolve(Op1, Vars, Funcs),
    {OpRes2, _Vars} = resolve(Op2, Vars, Funcs),
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

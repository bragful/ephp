-module(ephp_context).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile([export_all, warnings_as_errors]).

-include("ephp.hrl").

-record(state, {
    vars = ?DICT:new() :: dict(),
    funcs = ?DICT:new() :: dict(),
    timezone = "Europe/Madrid" :: string(),
    output = <<>> :: binary()
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

get_output(Context) ->
    gen_server:call(Context, output).

set_output(Context, Text) ->
    gen_server:cast(Context, {output, Text}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}};

init([#state{}=State]) ->
    {ok, State}.

handle_call({get, VarRawPath}, _From, State) ->
    {Value, NewState} = resolve_var(VarRawPath, State),
    {reply, Value, NewState};

handle_call({resolve, Expression}, _From, State) ->
    {Value, NewState} = resolve(Expression,State),
    {reply, Value, NewState};

handle_call({call, PHPFunc, Args}, _From, #state{funcs=Funcs}=State) ->
    {reply, case ?DICT:find(PHPFunc, Funcs) of
        error -> throw(eundefun);
        {ok, {Module, Fun}} -> {Module, Fun, Args}
    end, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(get_tz, _From, #state{timezone=TZ}=State) ->
    {reply, TZ, State};

handle_call(output, _From, #state{output=Output}=State) ->
    {reply, Output, State#state{output = <<>>}};

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

handle_cast({output, Text}, State) ->
    {noreply, do_output(State, Text)};

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

do_output(#state{output=Output}=State, Text) ->
    State#state{output = <<Output/binary, Text/binary>>}.


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


resolve(true, State) -> 
    {true, State};

resolve(false, State) -> 
    {false, State};

resolve(null, State) -> 
    {undefined, State};

resolve(#assign{variable=Var,expression=Expr}, State) ->
    VarPath = get_var_path(Var, State),
    {Value, NState} = resolve(Expr, State),
    NewVars = change(VarPath, Value, NState#state.vars),
    NewState = NState#state{vars = NewVars},
    {Value, NewState};

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

resolve({pre_incr, Var}, #state{vars=Vars}=State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, Vars) of
        undefined -> 
            {1, State#state{vars = change(VarPath, 1, Vars)}};
        V when is_number(V) -> 
            {V+1, State#state{vars = change(VarPath, V+1, Vars)}};
        V when is_binary(V) andalso byte_size(V) > 0 ->
            NewVal = try
                list_to_integer(binary_to_list(V)) + 1
            catch error:badarg ->
                ephp_util:increment_code(V)
            end,
            {NewVal, State#state{vars = change(VarPath, NewVal, Vars)}};
        V -> 
            {V, State}
    end;

resolve({pre_decr, Var}, #state{vars=Vars}=State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, Vars) of
        undefined -> 
            {undefined, State};
        V when is_number(V) -> 
            {V-1, State#state{vars = change(VarPath, V-1, Vars)}};
        V -> 
            {V, State}
    end;

resolve({post_incr, Var}, #state{vars=Vars}=State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, Vars) of
        undefined -> 
            {undefined, State#state{vars = change(VarPath, 1, Vars)}};
        V when is_number(V) -> 
            {V, State#state{vars = change(VarPath, V+1, Vars)}};
        V when is_binary(V) andalso byte_size(V) > 0 ->
            NewVal = try
                list_to_integer(binary_to_list(V)) + 1
            catch error:badarg ->
                ephp_util:increment_code(V)
            end,
            {V, State#state{vars = change(VarPath, NewVal, Vars)}};
        V -> 
            {V, State}
    end;

resolve({post_decr, Var}, #state{vars=Vars}=State) ->
    VarPath = get_var_path(Var, State),
    case search(VarPath, Vars) of
        undefined -> 
            {undefined, State};
        V when is_number(V) -> 
            {V, State#state{vars = change(VarPath, V-1, Vars)}};
        V -> 
            {V, State}
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

resolve(#call{name=Fun,args=RawArgs}, #state{funcs=Funcs}=State) ->
    case ?DICT:find(Fun, Funcs) of
        error -> throw(eundefun);
        {ok,{M,F}} -> 
            {Args, NState} = lists:foldl(fun(Arg,{Args,S}) ->
                {A,NewState} = resolve(Arg,S),
                {Args ++ [A], NewState}
            end, {[], State}, RawArgs),
            {ok, Mirror} = start_link(NState),
            Value = erlang:apply(M,F,[Mirror|Args]),
            MirrorState = get_state(Mirror),
            destroy(Mirror),
            {Value, MirrorState}
    end;

resolve(Unknown, #state{}=State) ->
    io:format("~p - ~p~n", [Unknown,State]),
    throw(eundeftoken).


resolve_var(#variable{idx=[]}=Var, #state{vars=Vars}=State) ->
    {search(Var, Vars), State};

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
    %% TODO: resolve index could be modified the value if it's used ++ or --
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
    end, State2}.

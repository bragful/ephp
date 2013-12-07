-module(ephp_context).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile([export_all, warnings_as_errors]).

-define(IS_DICT(D), (is_tuple(D) andalso element(1,D) =:= dict)).

-record(state, {
    vars = dict:new()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    get/2,
    set/3,
    solve/2,
    destroy/1
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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call({get, VarRawPath}, _From, #state{vars=Vars}=State) ->
    {reply, resolve_var(VarRawPath, Vars), State};

handle_call({resolve, Expression}, _From, #state{vars=Vars}=State) ->
    {Value, NewVars} = resolve(Expression,Vars),
    {reply, Value, State#state{vars = NewVars}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({set, VarRawPath, Value}, #state{vars=Vars}=State) ->
    VarPath = get_var_path(VarRawPath, Vars),
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

search({var, Root, []}, Vars) ->
    case dict:find(Root, Vars) of
        error -> undefined;
        {ok, Value} -> Value
    end;

search({var, Root, [NewRoot|Idx]}, Vars) ->
    case dict:find(Root, Vars) of
        {ok, NewVars} when ?IS_DICT(NewVars) -> 
            search({var, NewRoot, Idx}, NewVars);
        _ -> 
            undefined
    end.


change({var, Root, []}, Value, Vars) ->
    dict:store(Root, Value, Vars);

change({var, Root, [NewRoot|Idx]}, Value, Vars) ->
    case dict:find(Root, Vars) of
    {ok, NewVars} when ?IS_DICT(NewVars) -> 
        dict:store(Root, change({var, NewRoot, Idx}, Value, NewVars), Vars);
    _ -> 
        dict:store(Root, change({var, NewRoot, Idx}, Value, dict:new()), Vars)
    end.


resolve({assign,Var,Expr}, Vars) ->
    VarPath = get_var_path(Var, Vars),
    {Value, NewVars} = resolve(Expr, Vars),
    {Value, change(VarPath, Value, NewVars)};

resolve({operation,_Type,_Op1,_Op2}=Op, Vars) ->
    {resolve_op(Op, Vars), Vars};

resolve({int, Int}, Vars) -> 
    {Int, Vars};

resolve({float, Float}, Vars) -> 
    {Float, Vars};

resolve({text, Text}, Vars) -> 
    {Text, Vars};

resolve({text_to_process, Text}, Vars) ->
    {resolve_txt(Text, Vars), Vars};

resolve({if_block,Cond,TrueBlock,FalseBlock}, Vars) ->
    case resolve_op(Cond, Vars) of
    true ->
        resolve(TrueBlock, Vars);
    false ->
        resolve(FalseBlock, Vars)
    end;

resolve({var,Root,Idx}, Vars) ->
    {resolve_var({var,Root,Idx}, Vars), Vars};

resolve({array,ArrayElements}, Vars) ->
    {_I,Array,NVars} = lists:foldl(fun
        ({array_element,auto,Element}, {I,Dict,V}) ->
            {Value, NewVars} = resolve(Element,V),
            {I+1,dict:store(I,Value,Dict),NewVars};
        ({array_element,I,Element}, {INum,Dict,V}) ->
            {Value, NewVars} = resolve(Element,V),
            {Idx, ReNewVars} = resolve(I,NewVars),
            if
            is_integer(Idx) ->  
                {Idx+1,dict:store(Idx,Value,Dict),ReNewVars};
            true ->
                {INum,dict:store(Idx,Value,Dict),ReNewVars}
            end
    end, {0,dict:new(),Vars}, ArrayElements),
    {Array, NVars};

resolve(_Unknown, _Vars) ->
    throw(eundeftoken).


resolve_var({var, Root, []}, Vars) ->
    search({var,Root,[]}, Vars);

resolve_var({var, Root, Indexes}, Vars) ->
    NewIndexes = lists:map(fun(Idx) ->
        {Value, _Vars} = resolve(Idx, Vars),
        Value
    end, Indexes),
    search({var, Root, NewIndexes}, Vars).


get_var_path({var, Root, []}, _Vars) ->
    {var,Root,[]};

get_var_path({var, Root, Indexes}, Vars) ->
    NewIndexes = lists:map(fun(Idx) ->
        {Value, _Vars} = resolve(Idx, Vars),
        Value
    end, Indexes),
    {var, Root, NewIndexes}.


resolve_txt({text_to_process, Texts}, Vars) ->
    lists:foldr(fun(Data, ResultTxt) ->
        {TextRaw,_Vars} = resolve(Data, Vars),
        Text = ephp_util:to_bin(TextRaw),
        <<Text/binary, ResultTxt/binary>>
    end, <<>>, Texts).


resolve_op({operation, Type, Op1, Op2}, Vars) ->
    {OpRes1, _Vars} = resolve(Op1, Vars),
    {OpRes2, _Vars} = resolve(Op2, Vars),
    case Type of
        <<"+">> -> OpRes1 + OpRes2;
        <<"-">> -> OpRes1 - OpRes2;
        <<"*">> -> OpRes1 * OpRes2;
        <<"/">> -> OpRes1 / OpRes2;
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

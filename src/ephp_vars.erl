-module(ephp_vars).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    get/2,
    get/3,
    set/3,
    ref/4,
    del/2,
    zip_args/4,
    destroy/1
]).

-record(vars, {
    vars = ephp_array:new() :: ephp_array(),
    data :: reference()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(DataID) ->
    Ref = make_ref(),
    erlang:put(Ref, #vars{data=DataID}),
    {ok, Ref}.

get(Context, VarPath) ->
    get(Context, VarPath, undefined).

get(Context, VarPath, ContextRef) ->
    search(VarPath, (erlang:get(Context))#vars.vars, ContextRef).

set(Context, VarPath, Value) ->
    #vars{vars=Vars} = VarsSt = erlang:get(Context),
    NewVars = change(VarPath, Value, VarsSt)
    erlang:put(Context, VarsSt#vars{vars=NewVars}),
    ok.

ref(Context, VarPath, VarsPID, RefVarPath) ->
    ValueFormatted = #var_ref{pid=VarsPID, ref=RefVarPath},
    set(Context, VarPath, ValueFormatted).

del(Context, VarPath) ->
    set(Context, VarPath, undefined).

zip_args(VarsSrc, VarsDst, ValArgs, FuncArgs) ->
    lists:foldl(fun
        (#ref{var=VarRef}, [{VarName,_}|RestArgs]) ->
            ref(VarsDst, VarRef, VarsSrc, VarName),
            RestArgs;
        (FuncArg, [{_,ArgVal}|RestArgs]) ->
            set(VarsDst, FuncArg, ArgVal),
            RestArgs;
        (#variable{default_value=Val}=FuncArg, []) when Val =/= null ->
            set(VarsDst, FuncArg, Val),
            [];
        (_FuncArg, []) ->
            []
    end, ValArgs, FuncArgs),
    ok.

destroy(Context) ->
    erlang:erase(Context).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

search(#variable{name = <<"GLOBALS">>, idx=[]}, Vars, _Context) ->
    Vars;

search(#variable{name = <<"GLOBALS">>, idx=[Root|Idx]}, Vars, _Context) ->
    search(#variable{name=Root, idx=Idx}, Vars, undefined);

search(#variable{name=Root, idx=[], line=Line}, Vars, Context) ->
    case ephp_array:find(Root, Vars) of
    error when Context =:= undefined ->
        undefined;
    error ->
        File = ephp_context:get_active_file(Context),
        ephp_error:handle_error(Context,
            {error, eundefvar, Line, ?E_NOTICE, {File, Root}}),
        undefined;
    {ok, #var_ref{pid=RefVarsPID, ref=RefVar}} ->
        get(RefVarsPID, RefVar, undefined);
    {ok, Value} ->
        Value
    end;

search(#variable{name=Root, idx=[NewRoot|Idx], line=Line}, Vars, Context) ->
    case ephp_array:find(Root, Vars) of
    {ok, #var_ref{pid=RefVarsPID, ref=#variable{idx=NewIdx}=RefVar}} ->
        NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
        get(RefVarsPID, NewRefVar, undefined);
    {ok, #reg_instance{context=Ctx}} ->
        NewObjVar = #variable{name=NewRoot, idx=Idx},
        get(Ctx, NewObjVar, undefined);
    {ok, NewVars} ->
        search(#variable{name=NewRoot, idx=Idx}, NewVars, undefined);
    _ when Context =:= undefined ->
        undefined;
    _ ->
        File = ephp_context:get_active_file(Context),
        ephp_error:handle_error(Context,
            {error, eundefvar, Line, ?E_NOTICE, {File, Root}}),
        undefined
    end.

change(#variable{name = <<"GLOBALS">>, idx=[]}, Value, _Vars) when ?IS_ARRAY(Value) ->
    ephp_array:fold(fun(Root, Val, NewVars) ->
        ephp_array:store(Root, Val, NewVars)
    end, ephp_array:new(), Value);

change(#variable{name = <<"GLOBALS">>, idx=[Root|Idx]}, Value, Vars) ->
    change(#variable{name=Root, idx=Idx}, Value, Vars);

change(#variable{name=Root, idx=[]}=_Var, undefined, Vars) ->
    ephp_array:erase(Root, Vars);

change(#variable{name=Root, idx=[]}=_Var, Value, Vars) ->
    case ephp_array:find(Root, Vars) of
    {ok, #var_ref{pid=RefVarsPID, ref=RefVar}} ->
        set(RefVarsPID, RefVar, Value),
        Vars;
    _ ->
        ephp_array:store(Root, Value, Vars)
    end;

change(#variable{name=Root, idx=[{object,NewRoot,_Line}]}=_Var, Value, Vars) ->
    {ok, #reg_instance{context=Ctx}} = ephp_array:find(Root, Vars),
    ephp_context:set(Ctx, #variable{name=NewRoot}, Value),
    Vars;

change(#variable{name=Root, idx=[{object,NewRoot,_Line}|Idx]}=_Var, Value, Vars) ->
    {ok, #reg_instance{context=Ctx}} = ephp_array:find(Root, Vars),
    ephp_context:set(Ctx, #variable{name=NewRoot, idx=Idx}, Value),
    Vars;

change(#variable{name=Root, idx=[NewRoot|Idx]}=_Var, Value, Vars) ->
    case ephp_array:find(Root, Vars) of
    {ok, #var_ref{pid=RefVarsPID, ref=#variable{idx=NewIdx}=RefVar}} ->
        NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
        set(RefVarsPID, NewRefVar, Value),
        Vars;
    {ok, #reg_instance{context=Ctx}} ->
        ephp_context:set(Ctx, #variable{name=NewRoot, idx=Idx}, Value),
        Vars;
    {ok, NewVars} when ?IS_ARRAY(NewVars) ->
        ephp_array:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value,
            NewVars), Vars);
    _ ->
        ephp_array:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value,
            ephp_array:new()), Vars)
    end.

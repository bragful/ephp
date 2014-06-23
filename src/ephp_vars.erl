-module(ephp_vars).

-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    get/2,
    set/3,
    ref/4,
    del/2,
    destroy/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, ?DICT:new()),
    {ok, Ref}.

get(Context, VarPath) ->
    search(VarPath, erlang:get(Context)).

set(Context, VarPath, Value) ->
    erlang:put(Context, change(VarPath, Value, erlang:get(Context))),
    ok.

ref(Context, VarPath, VarsPID, RefVarPath) ->
    ValueFormatted = #var_ref{pid=VarsPID, ref=RefVarPath},
    set(Context, VarPath, ValueFormatted).

del(Context, VarPath) ->
    set(Context, VarPath, undefined).

destroy(Context) ->
    erlang:erase(Context).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

search(#variable{name = <<"GLOBALS">>, idx=[]}, Vars) ->
    Vars;

search(#variable{name = <<"GLOBALS">>, idx=[Root|Idx]}, Vars) ->
    search(#variable{name=Root, idx=Idx}, Vars);

search(#variable{name=Root, idx=[]}, Vars) ->
    case ?DICT:find(Root, Vars) of
        error ->
            undefined;
        {ok, #var_ref{pid=RefVarsPID, ref=RefVar}} ->
            get(RefVarsPID, RefVar);
        {ok, Value} ->
            Value
    end;

search(#variable{name=Root, idx=[NewRoot|Idx]}, Vars) ->
    case ?DICT:find(Root, Vars) of
        {ok, #var_ref{pid=RefVarsPID, ref=#variable{idx=NewIdx}=RefVar}} ->
            NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
            get(RefVarsPID, NewRefVar);
        {ok, NewVars} -> 
            search(#variable{name=NewRoot, idx=Idx}, NewVars);
        _ -> 
            undefined
    end.

change(#variable{name = <<"GLOBALS">>, idx=[]}, Value, _Vars) when ?IS_DICT(Value) ->
    lists:foldl(fun({Root,Val}, NewVars) ->
        ?DICT:store(Root, Val, NewVars)
    end, ?DICT:new(), ?DICT:to_list(Value));

change(#variable{name = <<"GLOBALS">>, idx=[Root|Idx]}, Value, Vars) ->
    change(#variable{name=Root, idx=Idx}, Value, Vars);

change(#variable{name=Root, idx=[]}=_Var, undefined, Vars) ->
    ?DICT:erase(Root, Vars);

change(#variable{name=Root, idx=[]}=_Var, Value, Vars) ->
    case ?DICT:find(Root, Vars) of
    {ok, #var_ref{pid=RefVarsPID, ref=RefVar}} ->
        set(RefVarsPID, RefVar, Value),
        Vars;
    _ ->
        ?DICT:store(Root, Value, Vars)
    end;

change(#variable{name=Root, idx=[NewRoot|Idx]}=_Var, Value, Vars) ->
    case ?DICT:find(Root, Vars) of
    {ok, #var_ref{pid=RefVarsPID, ref=#variable{idx=NewIdx}=RefVar}} ->
        NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
        set(RefVarsPID, NewRefVar, Value),
        Vars;
    {ok, NewVars} when ?IS_DICT(NewVars) -> 
        ?DICT:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value, NewVars), Vars);
    _ -> 
        ?DICT:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value, ?DICT:new()), Vars)
    end.

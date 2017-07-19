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
    isset/2,
    ref/4,
    del/2,
    zip_args/6,
    destroy/2,
    destroy_data/2
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    %% FIXME : PHP use a table for variables for hard reference to the
    %%         values so, when you use a reference, this creates a link
    %%         to the data instead of a link to the original variable.
    %%         The behaviour is, when you 'unset' a variable, you only
    %%         remove the link to the data and, if the data has 0 links
    %%         then the data is removed.
    Ref = make_ref(),
    erlang:put(Ref, ephp_array:new()),
    {ok, Ref}.

get(Vars, VarPath) ->
    get(Vars, VarPath, undefined).

get(Vars, VarPath, Context) ->
    search(VarPath, erlang:get(Vars), Context).

isset(Vars, VarPath) ->
    exists(VarPath, erlang:get(Vars)).

set(Vars, VarPath, Value) ->
    erlang:put(Vars, change(VarPath, Value, erlang:get(Vars))),
    ok.

ref(Vars, VarPath, VarsPID, RefVarPath) ->
    ValueFormatted = #var_ref{pid=VarsPID, ref=RefVarPath},
    set(Vars, VarPath, ValueFormatted).

del(Vars, VarPath) ->
    set(Vars, VarPath, remove).

zip_args(VarsSrc, VarsDst, ValArgs, FuncArgs, FunctName, Line) ->
    Zip = fun
        (#ref{var = VarRef}, [{#variable{} = VarName,_}|RestArgs]) ->
            ref(VarsDst, VarRef, VarsSrc, VarName),
            RestArgs;
        (#ref{}, _) ->
            ephp_error:error({error, enorefvar, Line, ?E_ERROR, {}});
        (FuncArg, [{_, ArgVal}|RestArgs]) ->
            set(VarsDst, FuncArg, ArgVal),
            RestArgs;
        (#variable{default_value = Val} = FuncArg, []) when Val =/= undefined ->
            set(VarsDst, FuncArg, Val),
            [];
        (_FuncArg, []) ->
            []
    end,
    Check = fun
        (_I, _Type, _Data, true) ->
            ok;
        (I, Type, Data, false) ->
            ephp_error:error({error, errtype, Line, ?E_RECOVERABLE_ERROR,
                              {I, Type, ephp_data:gettype(Data), FunctName}})
    end,
    lists:foldl(fun
        (#ref{var = #variable{data_type = DataType}} = Ref,
         {I, [{_, Value}|_] = Acc}) when DataType =/= undefined ->
            Check(I, DataType, Value, ephp_class:instance_of(Value, DataType)),
            {I+1, Zip(Ref, Acc)};
        (#variable{data_type = DataType} = Var,
         {I, [{_, Value}|_] = Acc}) when DataType =/= undefined ->
            Check(I, DataType, Value, ephp_class:instance_of(Value, DataType)),
            {I+1, Zip(Var, Acc)};
        (VarOrRef, {I, Acc}) ->
            {I+1, Zip(VarOrRef, Acc)}
    end, {1, ValArgs}, FuncArgs),
    ok.

-spec destroy(context(), ephp:vars_id()) -> ok.

destroy(Ctx, VarsRef) ->
    Vars = erlang:get(VarsRef),
    destroy_data(Ctx, Vars),
    erlang:erase(VarsRef),
    ok.

destroy_data(_Context, undefined) ->
    ok;

destroy_data(Context, ObjRef) when ?IS_OBJECT(ObjRef) ->
    ephp_object:remove_link(Context, ObjRef);

destroy_data(Context, Vars) when ?IS_ARRAY(Vars) ->
    ephp_array:fold(fun(_K, ObjRef, _) when ?IS_OBJECT(ObjRef) ->
                        destroy_data(Context, ObjRef);
                       (_K, V, _) when ?IS_ARRAY(V) ->
                        destroy_data(Context, V);
                       (_K, _V, _) ->
                        ok
                    end, undefined, Vars),
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

exists(#variable{name = Root, idx=[]}, Vars) ->
    case ephp_array:find(Root, Vars) of
        error -> false;
        {ok, undefined} -> false;
        _ -> true
    end;

exists(#variable{name = Root, idx=[NewRoot|Idx]}, Vars) ->
    case ephp_array:find(Root, Vars) of
        {ok, #var_ref{ref=global}} ->
            exists(#variable{name = NewRoot, idx = Idx}, Vars);
        {ok, #var_ref{pid=RefVarsPID, ref=#variable{idx=NewIdx}=RefVar}} ->
            NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
            isset(RefVarsPID, NewRefVar);
        {ok, #obj_ref{pid=Objects, ref=ObjectId}} ->
            Ctx = ephp_object:get_context(Objects, ObjectId),
            NewObjVar = #variable{name=NewRoot, idx=Idx},
            isset(Ctx, NewObjVar);
        {ok, NewVars} ->
            exists(#variable{name=NewRoot, idx=Idx}, NewVars);
        error ->
            false
    end.

search(global, Vars, _Context) ->
    Vars;

search(#variable{idx = []}, undefined, undefined) ->
    undefined;

search(#variable{name = Root, idx = [], line = Line}, undefined, Context) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context,
        {error, eundefvar, Line, File, ?E_NOTICE, {Root}}),
    undefined;

search(#variable{name = Root, idx = [], line = Line}, Vars, Context) ->
    case ephp_array:find(Root, Vars) of
        error when Context =:= undefined ->
            undefined;
        error ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefvar, Line, File, ?E_NOTICE, {Root}}),
            undefined;
        {ok, #var_ref{ref=global}} ->
            Vars;
        {ok, #var_ref{pid=RefVarsPID, ref=RefVar}} ->
            get(RefVarsPID, RefVar);
        {ok, Value} ->
            Value
    end;

search(#variable{name=Root, idx=[NewRoot|Idx], line=Line}, Vars, Context) ->
    case ephp_array:find(Root, Vars) of
        {ok, #var_ref{ref=global}} ->
            search(#variable{name = NewRoot, idx = Idx}, Vars, undefined);
        {ok, #var_ref{pid=RefVarsPID, ref=#variable{idx=NewIdx}=RefVar}} ->
            NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
            get(RefVarsPID, NewRefVar);
        {ok, #obj_ref{pid = Objects, ref = ObjectId}} ->
            Ctx = ephp_object:get_context(Objects, ObjectId),
            NewObjVar = #variable{name = NewRoot, idx = Idx},
            get(Ctx, NewObjVar);
        {ok, NewVars} ->
            search(#variable{name = NewRoot, idx = Idx}, NewVars, undefined);
        _ when Context =:= undefined ->
            undefined;
        _ ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefvar, Line, File, ?E_NOTICE, {Root}}),
            undefined
    end.

change(#variable{name=Root, idx=[]}=_Var, remove, Vars) ->
    ephp_array:erase(Root, Vars);

change(#variable{name=Root, idx=[]}=_Var, Value, Vars) ->
    if
        ?IS_OBJECT(Value) ->
            ephp_object:add_link(Value);
        true -> ok
    end,
    case ephp_array:find(Root, Vars) of
        {ok, #var_ref{ref=global}} ->
            ephp_array:store(Root, Value, Vars);
        {ok, #var_ref{pid=RefVarsPID, ref=RefVar}} ->
            set(RefVarsPID, RefVar, Value),
            Vars;
        _ ->
            ephp_array:store(Root, Value, Vars)
    end;

change(#variable{name=Root, idx=[{object,NewRoot,_Line}]}=_Var, Value, Vars) ->
    {ok, #obj_ref{ref=ObjectId, pid=Objects}} = ephp_array:find(Root, Vars),
    #ephp_object{context = Ctx} = RI = ephp_object:get(Objects, ObjectId),
    Class = ephp_class:add_if_no_exists_attrib(RI#ephp_object.class, NewRoot),
    NewRI = RI#ephp_object{class=Class},
    ephp_object:set(Objects, ObjectId, NewRI),
    ephp_context:set(Ctx, #variable{name=NewRoot}, Value),
    Vars;

change(#variable{name=Root, idx=[{object,NewRoot,_Line}|Idx]}=_Var,
       Value, Vars) ->
    {ok, #obj_ref{ref=ObjectId, pid=Objects}} = ephp_array:find(Root, Vars),
    #ephp_object{context = Ctx} = RI = ephp_object:get(Objects, ObjectId),
    Class = ephp_class:add_if_no_exists_attrib(RI#ephp_object.class, NewRoot),
    NewRI = RI#ephp_object{class=Class},
    ephp_object:set(RI#ephp_object.objects, RI#ephp_object.id, NewRI),
    ephp_context:set(Ctx, #variable{name=NewRoot, idx=Idx}, Value),
    Vars;

change(#variable{name=Root, idx=[NewRoot|Idx]}=_Var, Value, Vars) ->
    case ephp_array:find(Root, Vars) of
        {ok, #var_ref{ref=global}} ->
            change(#variable{name = NewRoot, idx = Idx}, Value, Vars);
        {ok, #var_ref{pid=RefVarsPID, ref=#variable{idx=NewIdx}=RefVar}} ->
            NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
            set(RefVarsPID, NewRefVar, Value),
            Vars;
        {ok, #obj_ref{pid = Objects, ref = ObjectId}} ->
            Ctx = ephp_object:get_context(Objects, ObjectId),
            ephp_context:set(Ctx, #variable{name=NewRoot, idx=Idx}, Value),
            Vars;
        {ok, NewVars} when ?IS_ARRAY(NewVars) ->
            ephp_array:store(Root,
                             change(#variable{name=NewRoot, idx=Idx}, Value,
                                    NewVars),
                             Vars);
        _ ->
            ephp_array:store(Root,
                             change(#variable{name=NewRoot, idx=Idx}, Value,
                                    ephp_array:new()),
                             Vars)
    end.

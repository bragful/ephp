-module(ephp_vars).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    clone/1,
    get/2,
    get/3,
    set/4,
    isset/3,
    ref/5,
    del/3,
    zip_args/7,
    destroy/2,
    destroy_data/2
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, ephp_array:new()),
    {ok, Ref}.

clone(Vars) ->
    NewVars = make_ref(),
    erlang:put(NewVars, erlang:get(Vars)),
    NewVars.

get(Vars, VarPath) ->
    get(Vars, VarPath, undefined).

get(Vars, VarPath, Context) ->
    search(VarPath, erlang:get(Vars), Context).

isset(Vars, VarPath, Context) ->
    exists(VarPath, erlang:get(Vars), Context).

set(Vars, VarPath, Value, Context) ->
    erlang:put(Vars, change(VarPath, Value, erlang:get(Vars), Context)),
    ok.

ref(Vars, VarPath, VarsPID, RefVarPath, Context) ->
    case get(VarsPID, RefVarPath) of
        Value when ?IS_OBJECT(Value) orelse ?IS_MEM(Value) ->
            set(Vars, VarPath, Value, Context);
        Value when RefVarPath =/= global ->
            MemRef = ephp_mem:add(Value),
            set(VarsPID, RefVarPath, MemRef, Context),
            set(Vars, VarPath, MemRef, Context);
        _ ->
            ValueFormatted = #var_ref{pid = VarsPID, ref = global},
            set(Vars, VarPath, ValueFormatted, Context)
    end.

del(Vars, VarPath, Context) ->
    set(Vars, VarPath, remove, Context).

zip_args(VarsSrc, VarsDst, ValArgs, FuncArgs, FunctName, Line, Context) ->
    Zip = fun
        (#ref{var = VarRef}, [{#variable{} = VarName,_}|RestArgs]) ->
            ref(VarsDst, VarRef, VarsSrc, VarName, Context),
            RestArgs;
        (#ref{}, _) ->
            ephp_error:error({error, enorefvar, Line, ?E_ERROR, {}});
        (FuncArg, [{_, ArgVal}|RestArgs]) ->
            set(VarsDst, FuncArg, ArgVal, Context),
            RestArgs;
        (#variable{default_value = Val} = FuncArg, []) when Val =/= undefined ->
            set(VarsDst, FuncArg, Val, Context),
            [];
        (_FuncArg, []) ->
            []
    end,
    Check = fun
        (_I, _Type, _Data, true) ->
            ok;
        (I, Type, Data, false) ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                                    {error, errtype, Line, File,
                                     ?E_RECOVERABLE_ERROR,
                                     {I, Type, ephp_data:gettype(Data), FunctName}})
    end,
    lists:foldl(fun
        (#ref{var = #variable{data_type = DataType}} = Ref,
         {I, [{_, Value}|_] = Acc}) when DataType =/= undefined ->
            Check(I, DataType, Value,
                  ephp_class:instance_of(Context, Value, DataType)),
            {I+1, Zip(Ref, Acc)};
        (#variable{data_type = DataType} = Var,
         {I, [{_, Value}|_] = Acc}) when DataType =/= undefined ->
            Check(I, DataType, Value,
                  ephp_class:instance_of(Context, Value, DataType)),
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
    ephp_object:remove(Context, ObjRef);

destroy_data(_Context, MemRef) when ?IS_MEM(MemRef) ->
    ephp_mem:remove(MemRef);

destroy_data(Context, Vars) when ?IS_ARRAY(Vars) ->
    ephp_array:fold(fun(_K, ObjRef, _) when ?IS_OBJECT(ObjRef) ->
                        destroy_data(Context, ObjRef);
                       (_K, V, _) when ?IS_ARRAY(V) ->
                        destroy_data(Context, V);
                       (_K, MemRef, _) when ?IS_MEM(MemRef) ->
                        destroy_data(Context, MemRef);
                       (_K, _V, _) ->
                        ok
                    end, undefined, Vars),
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

exists(#variable{} = Var, MemRef, Context) when ?IS_MEM(MemRef) ->
    exists(Var, ephp_mem:get(MemRef), Context);

exists(#variable{type = class, class = <<"self">>} = Var, _Vars, Context) ->
    ClassName = ephp_context:get_active_class(Context),
    Classes = ephp_context:get_classes(Context),
    case ephp_class:get(Classes, ClassName) of
        {ok, #class{static_context = ClassCtx}} ->
            ephp_context:isset(ClassCtx, Var#variable{type = normal});
        _ ->
            false
    end;

exists(#variable{type = class, class = <<"parent">>} = Var, _Vars, Context) ->
    ClassName = ephp_context:get_active_real_class(Context),
    Classes = ephp_context:get_classes(Context),
    case ephp_class:get(Classes, ClassName) of
        {ok, #class{extends = Parent}} ->
            ephp_context:isset(Context, Var#variable{class = Parent});
        _ ->
            false
    end;

exists(#variable{type = class, class = ClassName} = Var, _Vars, Context) ->
    Classes = ephp_context:get_classes(Context),
    case ephp_class:get(Classes, ClassName) of
        {ok, #class{static_context = ClassCtx}} ->
            ephp_context:isset(ClassCtx, Var#variable{type = normal});
        _ ->
            false
    end;

exists(#variable{name = Root, idx=[]}, Vars, _Context) ->
    case ephp_array:find(Root, Vars) of
        error -> false;
        {ok, undefined} -> false;
        _ -> true
    end;

exists(#variable{name = Root, idx=[NewRoot|Idx], line = Line}, Vars, Context)
        when ?IS_ARRAY(Vars) ->
    case ephp_array:find(Root, Vars) of
        {ok, #var_ref{ref=global}} ->
            exists(#variable{name = NewRoot, idx = Idx}, Vars, Context);
        {ok, #var_ref{pid=RefVarsPID, ref=#variable{idx=NewIdx}=RefVar}} ->
            NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
            isset(RefVarsPID, NewRefVar, Context);
        {ok, #obj_ref{} = ObjRef} ->
            Ctx = ephp_object:get_context(ObjRef),
            ActiveClass = ephp_context:get_active_class(Context),
            Classes = ephp_context:get_classes(Context),
            {ok, Class} = ephp_class:get(Classes, ActiveClass),
            {object, ObjRoot, _} = NewRoot,
            NewObjVar = case ephp_class:get_attribute(Class, ObjRoot) of
                #class_attr{access = private} ->
                    NewObjRoot = {private, ObjRoot, Class#class.name},
                    #variable{type = object, name = NewObjRoot, idx = Idx,
                              line = Line};
                _ ->
                    #variable{type = object, name = ObjRoot, idx = Idx,
                              line = Line}
            end,
            ObjVarsRef = ephp_context:get_vars(Ctx),
            isset(ObjVarsRef, NewObjVar, Context);
        {ok, NewVars} ->
            exists(#variable{name=NewRoot, idx=Idx}, NewVars, Context);
        error ->
            false
    end;

exists(#variable{idx = [_|_]}, Vars, _Context) when not ?IS_ARRAY(Vars) ->
    false.

search(global, Vars, _Context) ->
    Vars;

search(#variable{idx = []}, undefined, undefined) ->
    undefined;

search(#variable{name = Root, idx = [], line = Line, type = object,
                 class = ClassName},
       undefined, Context) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context,
        {error, eundefattr, Line, File, ?E_NOTICE, {Root, ClassName}}),
    undefined;

search(#variable{name = Root, idx = [], line = Line}, undefined, Context) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context,
        {error, eundefvar, Line, File, ?E_NOTICE, {Root}}),
    undefined;

search(#variable{name = Root, idx = [], line = Line, type = Type,
                 class = ClassName},
       Vars, Context) ->
    case ephp_array:find(Root, Vars) of
        error when Context =:= undefined ->
            undefined;
        error when Type =:= object ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefattr, Line, File, ?E_NOTICE, {Root, ClassName}}),
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

search(#variable{name = Root, idx = [NewRoot|Idx], line = Line, type = Type,
                 class = ClassName} = _Var,
       Vars, Context) ->
    case ephp_array:find(Root, Vars) of
        {ok, #var_ref{ref = global}} ->
            search(#variable{name = NewRoot, idx = Idx}, Vars, undefined);
        {ok, #var_ref{pid = RefVarsPID, ref = #variable{idx = NewIdx} = RefVar}} ->
            NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
            get(RefVarsPID, NewRefVar);
        {ok, MemRef} when ?IS_MEM(MemRef) ->
            search(#variable{name = NewRoot, idx = Idx},
                   ephp_mem:get(MemRef), Context);
        {ok, ObjRef} when ?IS_OBJECT(ObjRef) ->
            Classes = ephp_context:get_classes(Context),
            {object, ObjRoot, _} = NewRoot,
            NewObjVar = case ephp_context:get_active_class(Context) of
                <<>> ->
                    Class = ephp_object:get_class(ObjRef),
                    case ephp_class:get_attribute(Class, ObjRoot) of
                        #class_attr{access = private} ->
                            Data = {Class#class.name, ObjRoot, <<"private">>},
                            ephp_error:error({error, eprivateaccess, Line,
                                              ?E_ERROR, Data});
                        #class_attr{access = protected} ->
                            Data = {Class#class.name, ObjRoot, <<"protected">>},
                            ephp_error:error({error, eprivateaccess, Line,
                                              ?E_ERROR, Data});
                        _ ->
                            #variable{type = object, line = Line, name = ObjRoot,
                                      idx = Idx}
                    end;
                ActiveClass ->
                    {ok, Class} = ephp_class:get(Classes, ActiveClass),
                    case ephp_class:get_attribute(Class, ObjRoot) of
                        #class_attr{access = private} ->
                            NewObjRoot = {private, ObjRoot, Class#class.name},
                            #variable{type = object, line = Line,
                                      name = NewObjRoot, idx = Idx};
                        _ ->
                            #variable{type = object, line = Line, name = ObjRoot,
                                      idx = Idx}
                    end
            end,
            Ctx = ephp_object:get_context(ObjRef),
            ObjVars = erlang:get(ephp_context:get_vars(Ctx)),
            search(NewObjVar, ObjVars, Context);
        {ok, NewVars} ->
            search(#variable{name = NewRoot, idx = Idx}, NewVars, undefined);
        _ when Context =:= undefined ->
            undefined;
        _ when Type =:= object andalso ClassName =:= undefined ->
            Data = {ClassName, Root, <<"private">>},
            ephp_error:error({error, eprivateaccess, Line, ?E_ERROR, Data});
        _ when Type =:= object ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefattr, Line, File, ?E_NOTICE, {Root, ClassName}}),
            undefined;
        _ ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefvar, Line, File, ?E_NOTICE, {Root}}),
            undefined
    end.

-spec change(variable(), remove | mixed(), ephp:variables_id(), context()) ->
      ephp:variables_id().
%% @private
%% @doc change the value of a variable. This is used only internally.
change(#variable{name = Root, idx = []} = _Var, remove, Vars, Context) ->
    case ephp_array:find(Root, Vars) of
        {ok, ObjRef} when ?IS_OBJECT(ObjRef) ->
            ephp_object:remove(Context, ObjRef);
        {ok, MemRef} when ?IS_MEM(MemRef) ->
            ephp_mem:remove(MemRef);
        _ ->
            ok
    end,
    ephp_array:erase(Root, Vars);

change(#variable{name = auto, idx = []} = _Var, Value, Vars, _Context) ->
    if
        ?IS_OBJECT(Value) ->
            ephp_object:add_link(Value);
        ?IS_MEM(Value) ->
            ephp_mem:add_link(Value);
        true -> ok
    end,
    ephp_array:store(auto, Value, Vars);

change(#variable{name = Root, idx = []} = _Var, Value, Vars, Context) ->
    if
        ?IS_OBJECT(Value) ->
            ephp_object:add_link(Value);
        ?IS_MEM(Value) ->
            ephp_mem:add_link(Value);
        true -> ok
    end,
    case ephp_array:find(Root, Vars) of
        {ok, #var_ref{ref = global}} ->
            ephp_array:store(Root, Value, Vars);
        {ok, #var_ref{pid = RefVarsPID, ref = RefVar}} ->
            set(RefVarsPID, RefVar, Value, Context),
            Vars;
        {ok, ObjRef} when ?IS_OBJECT(ObjRef) ->
            ephp_object:remove(Context, ObjRef),
            ephp_array:store(Root, Value, Vars);
        {ok, MemRef} when (?IS_OBJECT(Value) orelse ?IS_MEM(Value)) andalso
                          ?IS_MEM(MemRef) ->
            ephp_mem:remove(MemRef),
            ephp_array:store(Root, Value, Vars);
        {ok, MemRef} when ?IS_MEM(MemRef) ->
            ephp_mem:set(MemRef, Value),
            Vars;
        _ ->
            ephp_array:store(Root, Value, Vars)
    end;

%% TODO: check when auto is passed as idx to trigger an error
change(#variable{name = <<"this">>, idx = [{object, NewRoot, _Line}|Idx]} = Var,
       Value, Vars, Context) ->
    {ok, #obj_ref{} = ObjRef} = ephp_array:find(<<"this">>, Vars),
    ObjCtx = ephp_object:get_context(ObjRef),
    ActiveClass = ephp_context:get_active_class(Context),
    Classes = ephp_context:get_classes(Context),
    {ok, Class} = ephp_class:get(Classes, ActiveClass),
    case NewRoot of
        {object, ObjRoot, _} -> ok;
        ObjRoot -> ok
    end,
    NewObjVar = case ephp_class:get_attribute(Class, ObjRoot) of
        #class_attr{access = private} ->
            NewObjRoot = {private, ObjRoot, Class#class.name},
            Var#variable{name = NewObjRoot, idx = Idx};
        _ ->
            ObjClass = ephp_object:get_class(ObjRef),
            NewClass = ephp_class:add_if_no_exists_attrib(ObjClass, NewRoot),
            ephp_object:set_class(ObjRef, NewClass),
            Var#variable{name = ObjRoot, idx = Idx}
    end,
    ephp_context:set(ObjCtx, NewObjVar, Value),
    Vars;

%% TODO: check when auto is passed as idx to trigger an error
change(#variable{name = Root, idx = [{object, NewRoot, _Line}|Idx]} = _Var,
       Value, Vars, _Context) ->
    {ok, #obj_ref{} = ObjRef} = ephp_array:find(Root, Vars),
    #ephp_object{context = Ctx, class = Class} = RI = ephp_object:get(ObjRef),
    NewClass = ephp_class:add_if_no_exists_attrib(Class, NewRoot),
    ephp_object:set(ObjRef, RI#ephp_object{class = NewClass}),
    ephp_context:set(Ctx, #variable{name = NewRoot, idx = Idx}, Value),
    Vars;

change(#variable{name = Root, idx = [NewRoot|Idx]} = Var, Value, Vars, Ctx) ->
    case ephp_array:find(Root, Vars) of
        {ok, #var_ref{ref = global}} ->
            change(#variable{name = NewRoot, idx = Idx}, Value, Vars, Ctx);
        {ok, #var_ref{pid = RefVarsPID, ref = #variable{idx = NewIdx} = RefVar}} ->
            NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
            set(RefVarsPID, NewRefVar, Value, Ctx),
            Vars;
        {ok, ObjRef} when ?IS_OBJECT(ObjRef) ->
            ObjCtx = ephp_object:get_context(ObjRef),
            ActiveClass = ephp_context:get_active_class(Ctx),
            Classes = ephp_context:get_classes(Ctx),
            {ok, Class} = ephp_class:get(Classes, ActiveClass),
            {object, ObjRoot, _} = NewRoot,
            NewObjVar = case ephp_class:get_attribute(Class, ObjRoot) of
                #class_attr{access = private} ->
                    NewObjRoot = {private, ObjRoot, Class#class.name},
                    Var#variable{name = NewObjRoot, idx = Idx};
                _ ->
                    Var#variable{name = ObjRoot, idx = Idx}
            end,
            ephp_context:set(ObjCtx, NewObjVar, Value),
            Vars;
        {ok, NewVars} when ?IS_ARRAY(NewVars) ->
            ephp_array:store(Root,
                             change(#variable{name = NewRoot, idx = Idx}, Value,
                                    NewVars, Ctx),
                             Vars);
        {ok, MemRef} when ?IS_MEM(MemRef) ->
            ephp_mem:set(MemRef, change(#variable{name=NewRoot, idx=Idx}, Value,
                                        ephp_mem:get(MemRef), Ctx)),
            Vars;
        _ ->
            ephp_array:store(Root,
                             change(#variable{name = NewRoot, idx = Idx}, Value,
                                    ephp_array:new(), Ctx),
                             Vars)
    end.

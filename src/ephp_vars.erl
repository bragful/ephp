-module(ephp_vars).
-author('manuel@altenwald.com').

-include("ephp.hrl").
-include("ephp_array.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    clone/1,
    variable/2,
    get/3,
    set/4,
    set_bulk/3,
    isset/3,
    empty/3,
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

variable(Name, Idx) ->
    #variable{name = Name, idx = Idx}.

get(Vars, VarPath, Context) ->
    search(VarPath, erlang:get(Vars), Context, true).

isset(Vars, VarPath, Context) ->
    exists(VarPath, erlang:get(Vars), Context).

empty(_Vars, undefined, _Context) -> true;
empty(_Vars, <<"0">>, _Context) -> true;
empty(_Vars, <<>>, _Context) -> true;
empty(_Vars, false, _Context) -> true;
empty(Vars, #variable{} = VarPath, Context) ->
    empty(Vars, search(VarPath, erlang:get(Vars), Context, false), Context);
empty(_Vars, _, _Context) -> false.

set_bulk(VarRef, VarVals, Context) ->
    Vars = lists:foldl(fun
        ({VarPath, Value}, Vars) when is_record(VarPath, variable) ->
            change(VarPath, Value, Vars, Context);
        ({VarName, Value}, Vars) when is_binary(VarName) ->
            change(#variable{name = VarName}, Value, Vars, Context);
        ({VarName, Idx, Value}, Vars) when is_binary(VarName) ->
            change(#variable{name = VarName, idx = Idx}, Value, Vars, Context)
    end, erlang:get(VarRef), VarVals),
    erlang:put(VarRef, Vars).

set(Vars, VarPath, Value, Context) ->
    erlang:put(Vars, change(VarPath, Value, erlang:get(Vars), Context)),
    ok.

ref(Vars, VarPath, VarsPID, RefVarPath, Context) ->
    case search(RefVarPath, erlang:get(VarsPID), Context, false) of
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
                  ephp_data:instance_of(Context, Value, DataType)),
            {I+1, Zip(Ref, Acc)};
        (#variable{data_type = DataType} = Var,
         {I, [{_, Value}|_] = Acc}) when DataType =/= undefined ->
            Check(I, DataType, Value,
                  ephp_data:instance_of(Context, Value, DataType)),
            {I+1, Zip(Var, Acc)};
        (VarOrRef, {I, Acc}) ->
            {I+1, Zip(VarOrRef, Acc)}
    end, {1, ValArgs}, FuncArgs),
    ok.

-spec destroy(ephp:context_id(), ephp:vars_id()) -> ok.

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

exists(#variable{}, undefined, _Context) ->
    false;

exists(#variable{type = class, class = <<"self">>} = Var, _Vars, Context) ->
    ClassName = ephp_context:get_active_class(Context),
    ClassNS = ephp_context:get_active_class_ns(Context),
    Classes = ephp_context:get_classes(Context),
    case ephp_class:get(Classes, ClassNS, ClassName) of
        {ok, #class{static_context = ClassCtx}} ->
            ephp_context:isset(ClassCtx, Var#variable{type = normal});
        _ ->
            false
    end;

exists(#variable{type = class, class = <<"parent">>} = Var, _Vars, Context) ->
    ClassName = ephp_context:get_active_real_class(Context),
    ClassNS = ephp_context:get_active_class_ns(Context),
    Classes = ephp_context:get_classes(Context),
    case ephp_class:get(Classes, ClassNS, ClassName) of
        {ok, #class{extends = Parent}} ->
            ephp_context:isset(Context, Var#variable{class = Parent});
        _ ->
            false
    end;

exists(#variable{type = class, class = ClassName, class_ns = ClassNS} = Var, _Vars, Context) ->
    Classes = ephp_context:get_classes(Context),
    case ephp_class:get(Classes, ClassNS, ClassName) of
        {ok, #class{static_context = ClassCtx}} ->
            ephp_context:isset(ClassCtx, Var#variable{type = normal});
        _ ->
            false
    end;

exists(#variable{name = Root, idx=[]}, Vars, _Context) ->
    case ephp_array:find(Root, Vars) of
        error -> false;
        {ok, undefined} -> false;
        {ok, MemRef} when ?IS_MEM(MemRef) ->
            case ephp_mem:get(MemRef) of
                undefined -> false;
                _ -> true
            end;
        _Value ->
            true
    end;

exists(#variable{name = Root, idx=[NewRoot|Idx], line = Line}, Vars, Context)
        when ?IS_ARRAY(Vars) ->
    case ephp_array:find(Root, Vars) of
        {ok, #var_ref{ref = global}} ->
            exists(#variable{name = NewRoot, idx = Idx}, Vars, Context);
        {ok, #var_ref{pid = RefVarsPID, ref = #variable{idx = NewIdx} = RefVar}} ->
            NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
            isset(RefVarsPID, NewRefVar, Context);
        {ok, #obj_ref{} = ObjRef} ->
            Ctx = ephp_object:get_context(ObjRef),
            ActiveClass = ephp_context:get_active_class(Context),
            ActiveClassNS = ephp_context:get_active_class_ns(Context),
            Classes = ephp_context:get_classes(Context),
            {ok, Class} = ephp_class:get(Classes, ActiveClassNS, ActiveClass),
            {object, ObjRoot, _} = NewRoot,
            NewObjVar = case ephp_class:get_attribute(Class, ObjRoot) of
                #class_attr{access = private,
                            class_name = AttrClassName,
                            namespace = AttrNS} ->
                    NewObjRoot = {private, ObjRoot, AttrNS, AttrClassName},
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


search(global, Vars, _Context, _Base) ->
    Vars;

search(#variable{idx = []}, undefined, undefined, _Base) ->
    undefined;

search(#variable{name = Name, idx = [NewName|Idx]} = Var, _Vars, Context, Base)
        when is_tuple(Name) andalso element(1, Name) =/= private ->
    case ephp_context:solve(Context, Name) of
        Name ->
            throw({error, {eloop, Name}});
        Vars ->
            search(Var#variable{name = NewName, idx = Idx}, Vars, Context, Base)
    end;

search(#variable{name = Root, idx = [], line = Line, type = object,
                 class = ClassName},
       undefined, Context, _Base) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context,
        {error, eundefattr, Line, File, ?E_NOTICE, {Root, ClassName}}),
    undefined;

search(#variable{name = Root, idx = [], line = Line, type = array},
       undefined, Context, _Base) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context,
        {error, eundefidx, Line, File, ?E_NOTICE, {Root}}),
    undefined;

search(#variable{name = Root, idx = [], line = Line}, undefined, Context,
       true) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context,
        {error, eundefvar, Line, File, ?E_NOTICE, {Root}}),
    undefined;

search(#variable{idx = []}, undefined, _Context, false) ->
    undefined;

% string handling: illegal string error
search(#variable{name = Root, line = Line} = Var, Vars, Context, Base)
        when is_binary(Vars) andalso not is_number(Root) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context, {error, eillegalstr, Line, File,
                                      ?E_WARNING, {Root}}),
    case Var#variable.idx of
        [] ->
            <<>>;
        [NewRoot|NewIdx] ->
            search(Var#variable{name = NewRoot, idx = NewIdx}, Vars,
                   Context, Base)
    end;

% string handling: cast error
search(#variable{name = Root, line = Line} = Var, Vars, Context, Base)
        when is_binary(Vars) andalso is_float(Root) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context, {error, estrcast, Line, File,
                                      ?E_NOTICE, {}}),
    search(Var#variable{name = ephp_data:to_int(Root)}, Vars, Context, Base);

% string handling: invalid index
search(#variable{name = Root, idx = Idx, line = Line} = Var, Vars, Context, Base)
        when is_binary(Vars) andalso (not is_integer(Root) orelse Root < 0) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context, {error, enoinitidx, Line, File, ?E_NOTICE,
                                      {Root}}),
    case Idx of
        [] ->
            <<>>;
        [NewRoot|NewIdx] ->
            search(Var#variable{name = NewRoot, idx = NewIdx}, Vars, Context, Base)
    end;

% string handling
search(#variable{name = Root, idx = Idx, line = Line} = Var, Vars, Context, Base)
        when is_binary(Vars) andalso is_integer(Root) andalso Root >= 0 ->
    case byte_size(Vars) =< Root of
        true ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefidxstr, Line, File, ?E_NOTICE, {Root}}),
            case Idx of
                [] ->
                    <<>>;
                [NewRoot|NewIdx] ->
                    search(Var#variable{name = NewRoot, idx = NewIdx},
                           Vars, Context, Base)
            end;
        false ->
            NewVars = binary_part(Vars, {Root, 1}),
            case Idx of
                [] ->
                    NewVars;
                [NewRoot|NewIdx] ->
                    search(Var#variable{name = NewRoot, idx = NewIdx},
                           NewVars, Context, Base)
            end
    end;

search(#variable{name = Root, idx = [], line = Line, type = Type,
                 class = ClassName},
       Vars, Context, Base) when ?IS_ARRAY(Vars) ->
    case ephp_array:find(Root, Vars) of
        error when not Base andalso Type =:= object ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefidx, Line, File, ?E_NOTICE, {Root}}),
            undefined;
        error when Context =:= undefined orelse not Base ->
            undefined;
        error when Type =:= object ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefattr, Line, File, ?E_NOTICE, {Root, ClassName}}),
            undefined;
        error when Type =:= array ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefidx, Line, File, ?E_NOTICE, {Root}}),
            undefined;
        error ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefvar, Line, File, ?E_NOTICE, {Root}}),
            undefined;
        {ok, #var_ref{ref = global}} ->
            Vars;
        {ok, #var_ref{pid = RefVarsPID, ref = RefVar}} ->
            search(RefVar, erlang:get(RefVarsPID), Context, false);
        {ok, Value} ->
            Value
    end;

search(#variable{name = Root, idx = [NewRoot|Idx], line = Line, type = Type,
                 class = ClassName} = Var,
       Vars, Context, Base) when ?IS_ARRAY(Vars) ->
    case translate_mem(ephp_array:find(Root, Vars)) of
        {ok, #var_ref{ref = global}} ->
            search(Var#variable{name = NewRoot, idx = Idx, type = array}, Vars, Context, false);
        {ok, #var_ref{pid = RefVarsPID, ref = #variable{idx = NewIdx} = RefVar}} ->
            NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
            search(NewRefVar, erlang:get(RefVarsPID), Context, false);
        {ok, ObjRef} when ?IS_OBJECT(ObjRef) ->
            {object, ObjRoot, _} = NewRoot,
            Ctx = ephp_object:get_context(ObjRef),
            ObjVars = erlang:get(ephp_context:get_vars(Ctx)),
            case ephp_context:get_active_class(Context) of
                <<>> when is_record(ObjRoot, call) ->
                    RealClassName = ephp_object:get_class_name(ObjRef),
                    Call = ObjRoot#call{class = RealClassName, type = object},
                    Value = ephp_context:call_method(Context, ObjRef, Call),
                    case Idx of
                        [H|T] ->
                            NewObjVar = #variable{type = object, line = Line,
                                                  name = H, idx = T},
                            %% FIXME: maybe Context and Base should be different?
                            search(NewObjVar, Value, Context, Base);
                        [] ->
                            Value
                    end;
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
                        #class_attr{} ->
                            NewObjVar = #variable{type = object, line = Line,
                                                  name = ObjRoot, idx = Idx},
                            search(NewObjVar, ObjVars, Context, Base);
                        undefined when Idx =:= [] ->
                            case ephp_class:get_method(Class, <<"__get">>) of
                                #class_method{} ->
                                    run_method_get(Context, ObjRef, NewRoot);
                                undefined ->
                                    NewObjVar = #variable{type = object, line = Line,
                                                          name = ObjRoot, idx = Idx},
                                    search(NewObjVar, ObjVars, Context, Base)
                            end;
                        undefined ->
                            case ephp_class:get_method(Class, <<"__get">>) of
                                #class_method{} ->
                                    NewVars = run_method_get(Context, ObjRef, NewRoot),
                                    [H|T] = Idx,
                                    NewVar = Var#variable{name = H, idx = T, type = normal},
                                    search(NewVar, NewVars, Context, true);
                                undefined ->
                                    NewObjVar = #variable{type = object, line = Line,
                                                          name = ObjRoot, idx = Idx},
                                    search(NewObjVar, ObjVars, Context, Base)
                            end
                    end;
                ActiveClass when is_record(ObjRoot, call) ->
                    Call = ObjRoot#call{class = ActiveClass, type = object},
                    Value = ephp_context:call_method(Context, ObjRef, Call),
                    case Idx of
                        [H|T] ->
                            NewObjVar = #variable{type = object, line = Line,
                                                  name = H, idx = T},
                            %% FIXME: maybe Context and Base should be different?
                            search(NewObjVar, Value, Context, Base);
                        [] ->
                            Value
                    end;
                ActiveClass ->
                    Classes = ephp_context:get_classes(Context),
                    ActiveClassNS = ephp_context:get_active_class_ns(Context),
                    {ok, Class} = ephp_class:get(Classes, ActiveClassNS, ActiveClass),
                    case ephp_class:get_attribute(Class, ObjRoot) of
                        #class_attr{access = private,
                                    class_name = AttrClassName,
                                    namespace = AttrNS} ->
                            NewObjRoot = {private, ObjRoot, AttrNS, AttrClassName},
                            NewObjVar = #variable{type = object, line = Line,
                                                  name = NewObjRoot, idx = Idx},
                            search(NewObjVar, ObjVars, Context, Base);
                        #class_attr{} ->
                            NewObjVar = #variable{type = object, line = Line,
                                                  name = ObjRoot, idx = Idx},
                            search(NewObjVar, ObjVars, Context, Base)
                    end
            end;
        {ok, NewVars} when ?IS_ARRAY(NewVars) ->
            search(Var#variable{name = NewRoot, idx = Idx, type = array}, NewVars,
                   Context, Base);
        {ok, NewVars} ->
            search(Var#variable{name = NewRoot, idx = Idx, type = array}, NewVars,
                   Context, Base);
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
        _ when Type =:= array ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefidx, Line, File, ?E_NOTICE, {Root}}),
            undefined;
        _Error ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefvar, Line, File, ?E_NOTICE, {Root}}),
            undefined
    end.

translate_mem({ok, MemRef}) when ?IS_MEM(MemRef) ->
    {ok, ephp_mem:get(MemRef)};
translate_mem(Other) -> Other.

run_method_get(Context, ObjRef, {object, Idx, _Line}) ->
    ClassName = ephp_object:get_class_name(ObjRef),
    Call = #call{name = <<"__get">>, class = ClassName, args = [Idx],
                 type = object},
    ephp_context:call_method(Context, ObjRef, Call).

run_method_set(Context, ObjRef, Idx, remove) ->
    ClassName = ephp_object:get_class_name(ObjRef),
    Call = #call{name = <<"__unset">>, class = ClassName, args = [Idx],
                 type = object},
    ephp_context:call_method(Context, ObjRef, Call);

run_method_set(Context, ObjRef, Idx, Value) ->
    ClassName = ephp_object:get_class_name(ObjRef),
    Call = #call{name = <<"__set">>, class = ClassName, args = [Idx, Value],
                 type = object},
    ephp_context:call_method(Context, ObjRef, Call).


-spec change(variable(), remove | mixed(), ephp:variables_id(), ephp:context_id()) ->
      ephp:variables_id().
%% @private
%% @doc change the value of a variable. This is used only internally.
change(#variable{name = Root, idx = []} = _Var, remove, Vars, Context) ->
    case ephp_array:find(Root, Vars) of
        {ok, ObjRef} when ?IS_OBJECT(ObjRef) ->
            ephp_object:remove(Context, ObjRef);
        {ok, MemRef} when ?IS_MEM(MemRef) ->
            ephp_mem:remove(MemRef);
        {ok, Array} when ?IS_ARRAY(Array) ->
            destroy_data(Context, Array);
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
        {ok, MemRef} when ?IS_OBJECT(Value) andalso ?IS_MEM(MemRef) ->
            ephp_mem:set(MemRef, Value),
            Vars;
        {ok, MemRef} when ?IS_MEM(Value) andalso ?IS_MEM(MemRef) ->
            ephp_mem:remove(MemRef),
            ephp_array:store(Root, Value, Vars);
        {ok, MemRef} when ?IS_MEM(MemRef) ->
            ephp_mem:set(MemRef, Value),
            Vars;
        _ ->
            ephp_array:store(Root, Value, Vars)
    end;

%% TODO: check when auto is passed as idx to trigger an error
change(#variable{name = <<"this">>, idx = [{object, ObjRoot, _Line}|Idx]} = Var,
       Value, Vars, Context) ->
    {ok, #obj_ref{} = ObjRef} = ephp_array:find(<<"this">>, Vars),
    ObjCtx = ephp_object:get_context(ObjRef),
    ActiveClass = ephp_context:get_active_class(Context),
    ActiveClassNS = ephp_context:get_active_class_ns(Context),
    Classes = ephp_context:get_classes(Context),
    {ok, Class} = ephp_class:get(Classes, ActiveClassNS, ActiveClass),
    NewObjVar = case ephp_class:get_attribute(Class, ObjRoot) of
        #class_attr{access = private,
                    class_name = AttrClassName,
                    namespace = AttrNS} ->
            NewObjRoot = {private, ObjRoot, AttrNS, AttrClassName},
            Var#variable{name = NewObjRoot, idx = Idx};
        _ ->
            ObjClass = ephp_object:get_class(ObjRef),
            NewClass = ephp_class:add_if_no_exists_attrib(ObjClass, ObjRoot),
            ephp_object:set_class(ObjRef, NewClass),
            Var#variable{name = ObjRoot, idx = Idx}
    end,
    ephp_context:set(ObjCtx, NewObjVar, Value),
    Vars;

%% TODO: check when auto is passed as idx to trigger an error
change(#variable{name = Root, idx = [{object, NewRoot, _Line}|Idx], line = Line},
       Value, Vars, Context) ->
    ObjRef = case ephp_array:find(Root, Vars) of
        {ok, ObRf} when ?IS_OBJECT(ObRf) -> ObRf;
        {ok, MemRef} when ?IS_MEM(MemRef) ->
            case ephp_mem:get(MemRef) of
                undefined ->
                    ephp_error:handle_error(Context, {error, eobj4empty, Line,
                                                      ephp_context:get_active_file(Context),
                                                      ?E_WARNING, undefined}),
                    Classes = ephp_context:get_classes(Context),
                    StdClass = ephp_class:instance(Classes, Context, Context, [], <<"stdClass">>, Line),
                    ephp_mem:set(MemRef, StdClass),
                    StdClass;
                ObRf -> ObRf
            end
    end,
    #ephp_object{context = Ctx, class = Class} = RI = ephp_object:get(ObjRef),
    SetMethod = ephp_class:get_method(Class, <<"__set">>),
    Attrib = ephp_class:get_attribute(Class, NewRoot),
    case {Attrib, SetMethod} of
        {#class_attr{}, _} ->
            ephp_context:set(Ctx, #variable{name = NewRoot, idx = Idx}, Value);
        {_, #class_method{}} when Idx =:= [] ->
            run_method_set(Ctx, ObjRef, NewRoot, Value);
        {_, #class_method{}} ->
            File = ephp_context:get_active_file(Context),
            ClassName = Class#class.name,
            ephp_error:handle_error(Context, {error, eindirectmod, Line, File,
                                              ?E_NOTICE, {NewRoot, ClassName}});
        {undefined, undefined} ->
            NewClass = ephp_class:add_if_no_exists_attrib(Class, NewRoot),
            ephp_object:set(ObjRef, RI#ephp_object{class = NewClass}),
            ephp_context:set(Ctx, #variable{name = NewRoot, idx = Idx}, Value)
    end,
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
            ActiveClassNS = ephp_context:get_active_class_ns(Ctx),
            Classes = ephp_context:get_classes(Ctx),
            {ok, Class} = ephp_class:get(Classes, ActiveClassNS, ActiveClass),
            {object, ObjRoot, _} = NewRoot,
            case ephp_class:get_attribute(Class, ObjRoot) of
                #class_attr{access = private,
                            class_name = AttrClassName,
                            namespace = AttrNS} ->
                    NewObjRoot = {private, ObjRoot, AttrNS, AttrClassName},
                    NewObjVar = Var#variable{name = NewObjRoot, idx = Idx},
                    ephp_context:set(ObjCtx, NewObjVar, Value),
                    Vars;
                #class_attr{} ->
                    NewObjVar = Var#variable{name = ObjRoot, idx = Idx},
                    ephp_context:set(ObjCtx, NewObjVar, Value),
                    Vars
            end;
        {ok, NewVars} when ?IS_ARRAY(NewVars) ->
            ephp_array:store(Root,
                             change(#variable{name = NewRoot, idx = Idx}, Value,
                                    NewVars, Ctx),
                             Vars);
        {ok, MemRef} when ?IS_MEM(MemRef) ->
            case ephp_mem:get(MemRef) of
                NewVars when ?IS_ARRAY(NewVars) ->
                    ephp_mem:set(MemRef,
                                 change(#variable{name = NewRoot, idx = Idx},
                                        Value, NewVars, Ctx));
                undefined when Value =:= remove ->
                    ok;
                undefined ->
                    ephp_mem:set(MemRef,
                                 change(#variable{name = NewRoot, idx = Idx},
                                        Value, ephp_array:new(), Ctx))
            end,
            Vars;
        _ ->
            ephp_array:store(Root,
                             change(#variable{name = NewRoot, idx = Idx}, Value,
                                    ephp_array:new(), Ctx),
                             Vars)
    end.

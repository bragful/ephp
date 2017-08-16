-module(ephp_class).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    destroy/1,

    get/2,
    get/3,
    get_constructor/2,
    get_destructor/2,
    get_attribute/2,
    get_clone/2,
    get_method/2,
    get_method/3,
    get_parent/2,

    class_attr/1,
    class_attr/2,
    class_attr/3,
    class_attr/4,

    init_static_value/5,
    set_static/4,

    instance_of/3,

    register_class/4,
    register_interface/3,
    set_alias/3,
    instance/5,

    get_stdclass/0,
    add_if_no_exists_attrib/2,
    register_loader/2
]).

-record(class_state, {
    classes = dict:new(),
    loaders = [] :: [callable()]
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, #class_state{}),
    ok = register_classes(Ref),
    {ok, Ref}.

destroy(Classes) ->
    erlang:erase(Classes),
    ok.

get(Context, ClassName, false) ->
    Classes = ephp_context:get_classes(Context),
    get(Classes, ClassName);
get(Context, ClassName, true) ->
    Classes = ephp_context:get_classes(Context),
    case erlang:get(Classes) of
        #class_state{loaders = []} ->
            get(Classes, ClassName);
        #class_state{loaders = Loaders} ->
            get(Context, Classes, ClassName, Loaders)
    end.

get(_Context, Classes, ClassName, []) ->
    get(Classes, ClassName);
get(Context, Classes, ClassName, [Loader|Loaders]) ->
    case get(Classes, ClassName) of
        {ok, Class} ->
            {ok, Class};
        {error, enoexist} ->
            %% TODO check callable for Loader
            Call = #call{name = Loader, args = [ClassName]},
            ephp_context:call_function(Context, Call),
            get(Context, Classes, ClassName, Loaders)
    end.

get(Ref, ClassName) ->
    #class_state{classes = Classes} = erlang:get(Ref),
    case dict:find(ClassName, Classes) of
        {ok, {alias, NewClassName}} ->
            get(Ref, NewClassName);
        {ok, Class} ->
            {ok, Class};
        error ->
            {error, enoexist}
    end.

set(Ref, ClassName, Class) ->
    #class_state{classes = Classes} = State = erlang:get(Ref),
    NC = dict:store(ClassName, Class, Classes),
    erlang:put(Ref, State#class_state{classes = NC}),
    ok.

set_alias(Ref, ClassName, AliasName) ->
    case get(Ref, ClassName) of
        {ok, _Class} ->
            case get(Ref, AliasName) of
                {ok, _} ->
                    {error, eredefined};
                {error, enoexist} ->
                    set(Ref, AliasName, {alias, ClassName})
            end;
        {error, enoexist} ->
            {error, enoexist}
    end.

register_loader(Ref, Loader) ->
    #class_state{loaders = Loaders} = State = erlang:get(Ref),
    erlang:put(Ref, State#class_state{loaders = [Loader|Loaders]}).

register_class(Ref, File, GlobalCtx,
               #class{name = Name, constants = ConstDef0,
                      methods = ClassMethods,
                      line = Index} = PHPClass) ->
    ConstDef = lists:flatmap(fun(I) ->
        case get(Ref, I) of
            {ok, #class{type = interface} = Interface} ->
                get_consts(Interface);
            _ ->
                ephp_error:error({error, enointerface, Index, ?E_ERROR, {I}})
        end
    end, PHPClass#class.implements) ++
         get_extends_consts(Ref, PHPClass) ++
         ConstDef0,
    Methods = extract_methods(Ref, Index, PHPClass#class.implements),
    Parents = extract_parents(Ref, PHPClass),
    case check_dup(PHPClass#class.implements) of
        ok ->
            ok;
        DupInterface ->
            ephp_error:error({error, edupinterface, Index, ?E_ERROR,
                              {Name, DupInterface}})
    end,
    case check_methods(Methods, PHPClass#class.methods) of
        [] ->
            ok;
        Errors when is_list(Errors) ->
            Params = length(Errors),
            Names = string:join(Errors, ", "),
            ephp_error:error({error, enomethods, Index, ?E_ERROR,
                              {Name, Names, Params}})
    end,
    case check_final_methods(Ref, PHPClass#class.methods, PHPClass#class.extends) of
        ok ->
            ok;
        {error, {Class, Method}} ->
            ephp_error:error({error, efinalmethod, Index, ?E_ERROR,
                              {Class, Method}})
    end,
    %% TODO: implement extends
    Consts = ephp_context:get_consts(GlobalCtx),
    ephp_const:set_bulk(Consts, Name, tr_consts(ConstDef, GlobalCtx)),
    %% TODO: create function to create better a context for classes/objects
    {ok, Ctx} = ephp_context:start_link(),
    ephp_context:set_active_file(Ctx, ephp_context:get_active_file(GlobalCtx)),
    ephp_context:set_errors_id(Ctx, ephp_context:get_errors_id(GlobalCtx)),
    ephp_context:set_output_handler(Ctx, ephp_context:get_output_handler(GlobalCtx)),
    ephp_context:set_global(Ctx, GlobalCtx),
    ActivePHPClass = PHPClass#class{
        static_context = Ctx,
        parents = Parents,
        file = File,
        methods = [ CM#class_method{class_name = Name} || CM <- ClassMethods ] ++
                  get_methods(Ref, PHPClass#class.extends),
        attrs = get_attrs(Ref, PHPClass)
    },
    initialize_class(ActivePHPClass),
    set(Ref, Name, ActivePHPClass),
    ok.

get_methods(_Classes, undefined) ->
    [];
get_methods(Classes, ClassName) ->
    {ok, #class{methods = Methods, extends = Parent}} = get(Classes, ClassName),
    Methods ++ get_methods(Classes, Parent).

get_attrs(_Classes, #class{name = Name, extends = undefined, attrs = Attrs}) ->
    attrs_set_class_name(Name, Attrs);
get_attrs(Classes, #class{name = Name, extends = Extends, attrs = Attrs}) ->
    %% TODO check if the inherited class doesn't exist
    {ok, Class} = get(Classes, Extends),
    %% TODO check duplicated
    attrs_set_class_name(Name, Attrs) ++  get_attrs(Classes, Class).

attrs_set_class_name(Name, Attrs) ->
    [ A#class_attr{class_name = Name} || A <- Attrs ].

tr_consts(Consts, Context) ->
    lists:map(fun(#class_const{name = N, value = V}) ->
        Val = ephp_context:solve(Context, V),
        {N, Val}
    end, Consts).

extract_parents(_Ref, undefined) ->
    [];
extract_parents(Ref, #class{extends = Extends, implements = Impl}) ->
    ParentExt = extract_parents(Ref, Extends),
    ParentImpl = lists:flatten([ extract_parents(Ref, I) || I <- Impl ]),
    ParentExt ++ ParentImpl;
extract_parents(Ref, Name) when is_binary(Name) ->
    {ok, Class} = get(Ref, Name),
    [Name|extract_parents(Ref, Class)].

extract_methods(Ref, Index, Implements) when is_reference(Ref) ->
    AllMethodsDict = lists:foldl(fun(I, D) ->
        {ok, #class{extends = Extends,
                    methods = ClassMethods}} = get(Ref, I),
        NewD = extract_methods(I, Index, ClassMethods, D),
        case Extends of
            undefined ->
                NewD;
            Parent ->
                {ok, #class{methods = ParentMethods}} = get(Ref, Parent),
                extract_methods(Parent, Index, ParentMethods, NewD)
        end
    end, [], lists:reverse(Implements)),
    [ V || {_,V} <- AllMethodsDict ].

extract_methods(_Name, _Index, [], MethodsDict) ->
    MethodsDict;
extract_methods(Name, Index, [Method|Methods], MethodsDict) ->
    #class_method{name = MethodName, args = Args} = Method,
    case lists:keyfind(MethodName, 1, MethodsDict) of
        false ->
            extract_methods(Name, Index, Methods,
                            [{MethodName, {Name, Method}}|MethodsDict]);
        {_, {PrevClassName, #class_method{args = PrevArgs}}} ->
            case length(PrevArgs) =:= length(Args) of
                true ->
                    extract_methods(Name, Index, Methods, MethodsDict);
                false ->
                    TxtArgs = ephp_string:join([ arg_to_text(Arg) || Arg <- Args ],
                                               <<", ">>),
                    ephp_error:error({error, eimpl, Index, ?E_ERROR,
                                      {PrevClassName, MethodName, Name, TxtArgs}})
            end
    end.

arg_to_text(#variable{name = Name}) -> <<"$", Name/binary>>;
arg_to_text(#var_ref{ref = #variable{name = Name}}) -> <<"$", Name/binary>>.

check_dup([]) -> ok;
check_dup([_Interface]) -> ok;
check_dup([Interface|Interfaces]) ->
    case lists:member(Interface, Interfaces) of
        true -> Interface;
        false -> check_dup(Interfaces)
    end.

check_methods(Interface, ClassMethods) ->
    lists:map(fun({IntName, #class_method{name = Name}}) ->
        io_lib:format("~s::~s", [IntName, Name])
    end, check_methods(Interface, ClassMethods, [])).

check_methods([], _ClassMethods, Error) ->
    Error;
check_methods([{ClassName,Method}|Methods], ClassMethods, Error) ->
    Res = lists:any(fun(#class_method{name = Name, args = Args}) ->
        (Name =:= Method#class_method.name) and
        (length(Args) =:= length(Method#class_method.args))
    end, ClassMethods),
    NewError = case Res of
        true -> Error;
        false -> [{ClassName, Method}|Error]
    end,
    check_methods(Methods, ClassMethods, NewError).

check_final_methods(_Ref, [], _Extends) ->
    ok;
check_final_methods(_Ref, _Methods, undefined) ->
    ok;
check_final_methods(Ref, Methods, ParentClass) ->
    {ok, #class{methods = ParentMethods,
                extends = Extends}} = get(Ref, ParentClass),
    case check_final_methods(Methods, ParentMethods) of
        ok -> check_final_methods(Ref, Methods, Extends);
        {error, MethodError} ->{error, {ParentClass, MethodError}}
    end.

check_final_methods([], _ParentMethods) ->
    ok;
check_final_methods([#class_method{name = MethodName}|Methods], ParentMethods) ->
    case lists:keyfind(MethodName, #class_method.name, ParentMethods) of
        #class_method{name = MethodName, final = true} ->
            {error, MethodName};
        _ ->
            check_final_methods(Methods, ParentMethods)
    end.

register_interface(Ref, File, #class{name = Name, line = Index,
                                     constants = Constants} = PHPInterface) ->
    #class_state{classes = Interfaces} = State = erlang:get(Ref),
    ConstDef = get_extends_consts(Ref, PHPInterface) ++ Constants,
    ActivePHPInterface = PHPInterface#class{file = File, constants = ConstDef},
    case dict:find(Name, Interfaces) of
        error ->
            NewClasses = dict:store(Name, ActivePHPInterface, Interfaces),
            NewState = State#class_state{classes = NewClasses},
            erlang:put(Ref, NewState),
            ok;
        {ok, _} ->
            ephp_error:error({error, eredefinedclass, Index, ?E_ERROR, {Name}})
    end.

get_extends_consts(_Ref, #class{extends = undefined}) ->
    [];
get_extends_consts(Ref, #class{name = Name, extends = Extends,
                               type = interface, line = Index}) ->
    case get(Ref, Extends) of
        {ok, #class{type = interface} = Interface} ->
            get_consts(Interface) ++ get_extends_consts(Ref, Interface);
        {ok, #class{}} ->
            ephp_error:error({error, ecannotimpl, Index, ?E_ERROR,
                              {Name, Extends}});
        _ ->
            ephp_error:error({error, enointerface, Index, ?E_ERROR, {Extends}})
    end;
get_extends_consts(Ref, #class{name = Name, extends = Extends,
                               line = Index}) ->
    case get(Ref, Extends) of
        {ok, #class{name = FName, final = true}} ->
            ephp_error:error({error, efinalclass, Index, ?E_ERROR,
                              {FName, Name}});
        {ok, #class{type = interface}} ->
            ephp_error:error({error, ecannotextends, Index, ?E_ERROR,
                              {Name, Extends}});
        {ok, #class{} = Class} ->
            get_consts(Class) ++ get_extends_consts(Ref, Class);
        _ ->
            ephp_error:error({error, enoclass, Index, ?E_ERROR, {Extends}})
    end.

instance(_Ref, LocalCtx, GlobalCtx, RawClassName, Line) ->
    case get(LocalCtx, RawClassName, true) of
    {ok, #class{} = Class} ->
        {ok, Ctx} = ephp_context:start_link(),
        ephp_context:set_active_file(Ctx, ephp_context:get_active_file(LocalCtx)),
        ephp_context:set_errors_id(Ctx, ephp_context:get_errors_id(LocalCtx)),
        ephp_context:set_output_handler(Ctx, ephp_context:get_output_handler(LocalCtx)),
        ephp_context:set_global(Ctx, GlobalCtx),
        RegClass = #ephp_object{class = Class, context = Ctx},
        Objects = ephp_context:get_objects(LocalCtx),
        ObjectId = ephp_object:add(Objects, RegClass),
        initialize(Ctx, Class#class.name, Class),
        #obj_ref{pid = Objects, ref = ObjectId};
    {error, enoexist} ->
        ephp_error:error({error, eundefclass, Line, ?E_ERROR,
                          {RawClassName}})
    end.

-spec instance_of(context(), mixed(), DataType::binary()) -> boolean().

instance_of(_Context, #ephp_array{}, <<"array">>) ->
    true;
instance_of(_Context, Boolean, <<"bool">>) when is_boolean(Boolean) ->
    true;
instance_of(_Context, String, <<"string">>) when is_binary(String) ->
    true;
instance_of(_Context, Float, <<"float">>) when is_float(Float) ->
    true;
instance_of(_Context, Int, <<"int">>) when is_integer(Int) ->
    true;
% TODO:
% instance_of(Callable, <<"callable">>) ->
%     false;
instance_of(Context, Self, <<"self">>) ->
    %% TODO scope error if active_class is not defined
    SelfClass = ephp_context:get_active_class(Context),
    instance_of(Context, Self, SelfClass);
instance_of(Context, ObjRef, Name) when ?IS_OBJECT(ObjRef) ->
    #ephp_object{class = Class} = ephp_object:get(ObjRef),
    #class{parents = Parents, name = InstanceName} = Class,
    Classes = ephp_context:get_classes(Context),
    case get(Classes, Name) of
        {ok, #class{name = ClassName}} ->
            lists:any(fun(F) -> F() end, [
                fun() -> ClassName =:= InstanceName end,
                fun() -> lists:member(ClassName, Parents) end
            ]);
        {error, _} ->
            false
    end;
instance_of(_Context, _Data, _Type) ->
    false.

initialize_class(#class{static_context=Ctx, attrs=Attrs}) ->
    lists:foreach(fun
        (#class_attr{type=static, name=Name, init_value=RawVal}) ->
            Val = ephp_context:solve(Ctx, RawVal),
            ephp_context:set(Ctx, #variable{name=Name}, Val);
        (#class_attr{type=normal}) ->
            ignore
    end, Attrs).

initialize(Ctx, ClassName, #class{attrs=Attrs}) ->
    lists:foreach(fun
        (#class_attr{type = normal, access = private, class_name = CName})
                when ClassName =/= CName ->
            ignore;
        (#class_attr{type = normal, name = Name, init_value = RawVal}) ->
            Val = ephp_context:solve(Ctx, RawVal),
            ephp_context:set(Ctx, #variable{name=Name}, Val);
        (#class_attr{type = static}) ->
            ignore
    end, Attrs).

get_constructor(Ref, #class{name = Name, methods = Methods,
                            extends = Extends}) ->
    MethodName = <<"__construct">>,
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        case lists:keyfind(Name, #class_method.name, Methods) of
        false ->
            case Extends of
                undefined ->
                    undefined;
                Extends ->
                    {ok, Parent} = get(Ref, Extends),
                    get_constructor(Ref, Parent)
            end;
        #class_method{}=ClassMethod ->
            ClassMethod
        end;
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

get_clone(Ref, #class{name = Name, methods = Methods, extends = Extends}) ->
    MethodName = <<"__clone">>,
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        case lists:keyfind(Name, #class_method.name, Methods) of
        false ->
            case Extends of
                undefined ->
                    undefined;
                Extends ->
                    {ok, Parent} = get(Ref, Extends),
                    get_clone(Ref, Parent)
            end;
        #class_method{}=ClassMethod ->
            ClassMethod
        end;
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

get_destructor(Ref, #class{methods = Methods, extends = Extends}) ->
    MethodName = <<"__destruct">>,
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        case Extends of
            undefined ->
                undefined;
            Extends ->
                {ok, Parent} = get(Ref, Extends),
                get_destructor(Ref, Parent)
        end;
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

get_attribute(#class{attrs=Attrs}, AttributeName) ->
    case lists:keyfind(AttributeName, #class_attr.name, Attrs) of
    false ->
        undefined;
    #class_attr{}=ClassAttr ->
        ClassAttr
    end.

get_method(#class{methods = Methods}, MethodName) ->
    case lists:keyfind(MethodName, #class_method.name, Methods) of
        false ->
            undefined;
        #class_method{} = ClassMethod ->
            ClassMethod
    end.

get_method(Class, Index, MethodName) ->
    case get_method(Class, MethodName) of
        undefined ->
            %% TODO: search "__call" method
            ephp_error:error({error, eundefmethod, Index, ?E_ERROR,
                              {Class#class.name, MethodName}});
        #class_method{} = ClassMethod ->
            ClassMethod
    end.

get_parent(Context, Name) ->
    Classes = ephp_context:get_classes(Context),
    {ok, #class{extends = Extends}} = get(Classes, Name),
    Extends.

init_static_value(Ref, ClassName, MethodName, VarName, Value) ->
    {ok, #class{methods = Methods}} = {ok, Class} = get(Ref, ClassName),
    case lists:keyfind(MethodName, #class_method.name, Methods) of
        #class_method{static = Static} = Method ->
            case orddict:find(VarName, Static) of
                {ok, RealValue} ->
                    RealValue;
                error ->
                    NewStatic = orddict:store(VarName, Value, Static),
                    NewMethod = Method#class_method{static = NewStatic},
                    NewMethods = lists:keyreplace(MethodName,
                                                  #class_method.name,
                                                  Methods,
                                                  NewMethod),
                    NewClass = Class#class{methods = NewMethods},
                    set(Ref, ClassName, NewClass),
                    Value
            end;
        false ->
            throw({error, enofunc})
    end.

set_static(Ref, ClassName, MethodName, Vars) ->
    {ok, #class{methods = Methods}} = {ok, Class} = get(Ref, ClassName),
    case lists:keyfind(MethodName, #class_method.name, Methods) of
        #class_method{static = Static} = Method ->
            NewStatic = lists:map(fun({Key, _}) ->
                %% TODO check behaviour when use unset
                NewValue = ephp_vars:get(Vars, #variable{name = Key}),
                {Key, NewValue}
            end, Static),
            NewMethod = Method#class_method{static = NewStatic},
            NewMethods = lists:keyreplace(MethodName,
                                          #class_method.name,
                                          Methods,
                                          NewMethod),
            NewClass = Class#class{methods = NewMethods},
            set(Ref, ClassName, NewClass);
        false when Class#class.extends =/= undefined ->
            set_static(Ref, Class#class.extends, MethodName, Vars)
    end,
    ok.

get_consts(#class{constants = Constants}) ->
    Constants.

add_if_no_exists_attrib(#class{attrs=Attrs}=Class, Name) ->
    case get_attribute(Class, Name) of
        undefined ->
            Class#class{
                attrs=Attrs ++ [#class_attr{name = Name}]
            };
        _ ->
            Class
    end.

get_stdclass() ->
    #class{
        name = <<"stdClass">>,
        constants = dict:new(),
        attrs = []
    }.

class_attr(Name) ->
    class_attr(Name, public, undefined, false).

class_attr(Name, Access) ->
    class_attr(Name, Access, undefined, false).

class_attr(Name, Access, InitValue) ->
    class_attr(Name, Access, InitValue, false).

class_attr(Name, Access, InitValue, Final) ->
    #class_attr{
        name = Name,
        access = Access,
        init_value = InitValue,
        final = Final
    }.

%% ------------------------------------------------------------------
%% Private functions
%% ------------------------------------------------------------------

register_classes(Ref) ->
    Classes = [
        get_stdclass(),
        get_traversable(),
        get_iterator(),
        get_iterator_aggregate(),
        get_array_access(),
        ephp_class_exception:get_class()
    ],
    State = erlang:get(Ref),
    ClassesDict = lists:foldl(fun(#class{name = Name} = Class, Dict) ->
        dict:store(Name, Class, Dict)
    end, State#class_state.classes, Classes),
    erlang:put(Ref, State#class_state{classes = ClassesDict}),
    ok.

get_traversable() ->
    #class{
        name = <<"Traversable">>,
        type = interface
    }.

get_iterator_aggregate() ->
    M = #class_method{type = abstract},
    #class{
        name = <<"IteratorAggregate">>,
        type = interface,
        extends = <<"Traversable">>,
        methods = [
            M#class_method{name = <<"getIterator">>}
        ]
    }.

get_iterator() ->
    M = #class_method{type = abstract},
    #class{
        name = <<"Iterator">>,
        type = interface,
        extends = <<"Traversable">>,
        methods = [
            M#class_method{name = <<"current">>},
            M#class_method{name = <<"key">>},
            M#class_method{name = <<"next">>},
            M#class_method{name = <<"rewind">>},
            M#class_method{name = <<"valid">>}
        ]
    }.

get_array_access() ->
    M = #class_method{type = abstract},
    MO = M#class_method{args = [#variable{name = <<"offset">>}]},
    MV = M#class_method{args = [#variable{name = <<"offset">>},
                                #variable{name = <<"value">>}]},
    #class{
        name = <<"ArrayAccess">>,
        type = interface,
        methods = [
            MO#class_method{name = <<"offsetExists">>},
            MO#class_method{name = <<"offsetGet">>},
            MV#class_method{name = <<"offsetSet">>},
            MO#class_method{name = <<"offsetUnset">>}
        ]
    }.


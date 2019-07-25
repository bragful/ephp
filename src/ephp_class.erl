%% @doc This module stores the classes found in the code and other defined
%%      classes here and defined in the `ephp_class_*' modules.
%%
%%      This module is responsible to handle that information as well.
%%      Retrieves the methods, constructor, destructor, parent class,
%%      attributes, and static data.
%%
%%      This module has the possibility to handle interfaces. The
%%      interfaces are handled in ephp as special classes but
%%      registering only methods and constants.
%%
%%      Finally this module has functions to create instances and
%%      check if a data is an instance of a specific class, interface
%%      or native data.
%% @end
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

    get/3,
    get/4,
    get_constructor/2,
    get_destructor/2,
    get_attribute/2,
    get_clone/2,
    get_method/2,
    get_method/3,
    get_parent/3,

    class_attr/1,
    class_attr/2,
    class_attr/3,
    class_attr/4,

    init_static_value/6,
    set_static/6,

    instance_of/3,

    register_class/4,
    register_classes/2,
    register_classes/3,
    register_interface/3,
    set_alias/5,
    instance/6,

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

-spec start_link() -> {ok, ephp:classes_id()}.
%% @doc starts a classes handler.
start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, #class_state{}),
    {ok, Ref}.


-spec destroy(ephp:classes_id()) -> ok.
%% @doc destroy the classes handler.
destroy(Classes) ->
    erlang:erase(Classes),
    ok.


-type get_return() :: {ok, class()} | {error, enoexist}.

-spec get(context(), namespace(), class_name(), AutoLoad::boolean()) -> get_return().
%% @doc retrieves a class registered given the class name.
get(Context, NS, ClassName, false) when is_list(NS) ->
    Classes = ephp_context:get_classes(Context),
    get(Classes, NS, ClassName);

get(Context, NS, ClassName, spl) when is_list(NS) ->
    Classes = ephp_context:get_classes(Context),
    case erlang:get(Classes) of
        #class_state{loaders = []} ->
            get(Classes, NS, ClassName);
        #class_state{loaders = Loaders} ->
            get(Context, Classes, NS, ClassName, Loaders)
    end;

get(Context, NS, ClassName, true) when is_list(NS) ->
    Classes = ephp_context:get_classes(Context),
    Funcs = ephp_context:get_funcs(Context),
    case ephp_func:is_defined(Funcs, <<"__autoload">>) of
        true ->
            case erlang:get(Classes) of
                #class_state{loaders = []} ->
                    get(Context, Classes, NS, ClassName, [<<"__autoload">>]);
                #class_state{loaders = Loaders} ->
                    get(Context, Classes, NS, ClassName, [<<"__autoload">>|Loaders])
            end;
        false ->
            get(Context, NS, ClassName, spl)
    end.


-type loader() :: binary().
-type loaders() :: [loader()].

-spec get(context(), ephp:classes_id(), namespace(), class_name(), loaders()) ->
      get_return().
%% @hidden
get(_Context, Classes, NS, ClassName, []) ->
    get(Classes, NS, ClassName);

get(Context, Classes, NS, ClassName, [Loader|Loaders]) ->
    case get(Classes, NS, ClassName) of
        {ok, Class} ->
            {ok, Class};
        {error, enoexist} ->
            %% TODO check callable for Loader
            FullClassName = ephp_ns:to_bin(NS, ClassName),
            Call = #call{name = Loader, args = [FullClassName]},
            ephp_context:call_function(Context, Call),
            get(Context, Classes, NS, ClassName, Loaders)
    end.


-spec get(ephp:classes_id(), namespace(), class_name()) -> get_return().
%% @doc retrieves a class registered given the class name.
get(Ref, NS, ClassName) ->
    #class_state{classes = Classes} = erlang:get(Ref),
    case dict:find({NS, ClassName}, Classes) of
        {ok, {alias, NewNS, NewClassName}} ->
            get(Ref, NewNS, NewClassName);
        {ok, Class} ->
            {ok, Class};
        error ->
            {error, enoexist}
    end.


-type alias_class() :: {alias, namespace(), class_name()}.

-spec set(ephp:classes_id(), namespace(), class_name(), class()) -> ok.
%% @doc adds a class using the class name inside of the handler.
set(Ref, NS, ClassName, #class{namespace = NS} = Class) ->
    #class_state{classes = Classes} = State = erlang:get(Ref),
    NC = dict:store({NS, ClassName}, Class, Classes),
    erlang:put(Ref, State#class_state{classes = NC}),
    ok;

set(Ref, NS, ClassName, {alias, _NSAlias, _ClassAlias} = Alias) ->
    #class_state{classes = Classes} = State = erlang:get(Ref),
    NC = dict:store({NS, ClassName}, Alias, Classes),
    erlang:put(Ref, State#class_state{classes = NC}),
    ok.

-type alias_return() :: {ok, class()} |
                        {error, enoexist | eredefined}.

-spec set_alias(ephp:classes_id(), namespace(), class_name(),
                namespace(), Alias :: alias_class()) -> alias_return().
%% @doc set a name as alias of a class name.
set_alias(Ref, NS, ClassName, NSAlias, AliasName) ->
    case get(Ref, NS, ClassName) of
        {ok, _Class} ->
            case get(Ref, NS, AliasName) of
                {ok, _} ->
                    {error, eredefined};
                {error, enoexist} ->
                    set(Ref, NSAlias, AliasName, {alias, NS, ClassName})
            end;
        {error, enoexist} ->
            {error, enoexist}
    end.


-spec register_loader(ephp:classes_id(), loader()) -> ok.
%% @doc register loader for the classes handler.
register_loader(Ref, Loader) ->
    #class_state{loaders = Loaders} = State = erlang:get(Ref),
    erlang:put(Ref, State#class_state{loaders = [Loader|Loaders]}),
    ok.


-spec register_class(ephp:classes_id(), File :: binary(), context(),
                     class()) -> ok.
%% @doc register a class inside of the classes handler.
register_class(Ref, File, GlobalCtx,
               #class{name = Name, constants = ConstDef0,
                      methods = ClassMethods, namespace = NS,
                      line = Index} = PHPClass) ->
    ConstDef = lists:flatmap(fun({INS, I}) ->
        case get(GlobalCtx, INS, I, true) of
            {ok, #class{type = interface} = Interface} ->
                get_consts(Interface);
            _ ->
                ephp_error:error({error, enointerface, Index, ?E_ERROR, {I}})
        end
    end, PHPClass#class.implements) ++
         get_extends_consts(GlobalCtx, PHPClass) ++
         ConstDef0,
    Methods = extract_methods(Ref, Index, PHPClass#class.implements),
    Parents = extract_parents(Ref, NS, PHPClass),
    case check_dup(PHPClass#class.implements) of
        ok ->
            ok;
        {DupInterfaceNS, DupInterfaceName} ->
            DupInterface = ephp_ns:to_bin(DupInterfaceNS, DupInterfaceName),
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
    case check_final_methods(Ref, PHPClass#class.methods,
                             PHPClass#class.extends_ns,
                             PHPClass#class.extends) of
        ok ->
            ok;
        {error, {Class, Method}} ->
            ephp_error:error({error, efinalmethod, Index, ?E_ERROR,
                              {Class, Method}})
    end,
    Consts = ephp_context:get_consts(GlobalCtx),
    ephp_const:set_bulk(Consts, NS, Name, tr_consts(ConstDef, GlobalCtx)),
    {ok, Ctx} = ephp_context:generate_subcontext(GlobalCtx),
    ActivePHPClass = PHPClass#class{
        static_context = Ctx,
        parents = Parents,
        file = File,
        methods = [ CM#class_method{class_name = Name,
                                    namespace = NS} || CM <- ClassMethods ] ++
                  get_methods(Ref, PHPClass#class.extends_ns, PHPClass#class.extends),
        attrs = get_attrs(Ref, PHPClass)
    },
    case check_access_level(ActivePHPClass#class.attrs) of
        ok -> ok;
        {error, {_Class, _Method, _Access, _ParentClass} = Data} ->
            ephp_error:error({error, eaccesslevel, Index, ?E_ERROR, Data})
    end,
    initialize_class(ActivePHPClass),
    set(Ref, NS, Name, ActivePHPClass),
    ok.


-spec check_access_level([class_method()]) ->
      ok | {error, {class_name(), binary(), access_types(), class_name()}}.
%% @doc check the access level for the overrided attributes.
check_access_level([]) ->
    ok;

check_access_level([#class_attr{access = Access, name = Name} = CA1|Rest]) ->
    case lists:keyfind(Name, #class_attr.name, Rest) of
        #class_attr{access = Access} ->
            check_access_level(Rest);
        #class_attr{} when Access =:= public ->
            check_access_level(Rest);
        #class_attr{access = OtherAccess} = CA2 ->
            {error, {CA1#class_attr.class_name, Name, OtherAccess,
                     CA2#class_attr.class_name}};
        false ->
            check_access_level(Rest)
    end.


-spec get_methods(ephp:classes_id(), namespace(), class_name() | undefined) ->
      [class_method()].
%% @doc retrieve all of the methods given a class name.
get_methods(_Classes, _NS, undefined) ->
    [];

get_methods(Classes, NS, ClassName) ->
    {ok, #class{methods = Methods,
                extends = Parent,
                extends_ns = ParentNS}} = get(Classes, NS, ClassName),
    Methods ++ get_methods(Classes, ParentNS, Parent).


-spec get_attrs(ephp:classes_id(), class()) -> [class_attr()].
%% @doc retrieve all of the attributes from a class registered in the handler.
get_attrs(_Classes, #class{name = Name, extends = undefined, attrs = Attrs}) ->
    attrs_set_class_name(Name, Attrs);

get_attrs(Classes, #class{name = Name, extends = Extends, attrs = Attrs,
                          extends_ns = ExtendsNS}) ->
    %% TODO check if the inherited class doesn't exist
    {ok, Class} = get(Classes, ExtendsNS, Extends),
    %% TODO check duplicated
    attrs_set_class_name(Name, Attrs) ++  get_attrs(Classes, Class).


-spec attrs_set_class_name(Name :: binary(), [class_attr()]) ->
      [class_attr()].
%% @doc set the class name for each class attribute.
attrs_set_class_name(Name, Attrs) ->
    [ A#class_attr{class_name = Name} || A <- Attrs ].


-spec tr_consts([class_const()], context()) -> [class_const()].
%% @doc solve the constants values.
tr_consts(Consts, Context) ->
    lists:map(fun(#class_const{name = N, value = V}) ->
        Val = ephp_context:solve(Context, V),
        {N, Val}
    end, Consts).


-spec extract_parents(ephp:classes_id(), namespace(), class() | undefined) ->
      [class()].
%% @doc retrieve a list with all of the parent class records.
extract_parents(_Ref, _NS, undefined) ->
    [];

extract_parents(Ref, _NS, #class{extends_ns = ExtendsNS,
                                 extends = Extends,
                                 implements = Impl}) ->
    ParentExt = extract_parents(Ref, ExtendsNS, Extends),
    ParentImpl = [ extract_parents(Ref, INS, I) || {INS, I} <- Impl ],
    lists:flatten(ParentExt ++ ParentImpl);

extract_parents(Ref, NS, Name) when is_binary(Name) ->
    {ok, Class} = get(Ref, NS, Name),
    [Name|extract_parents(Ref, NS, Class)].


-spec extract_methods(ephp:classes_id(), Index::mixed(), [class_name()]) ->
      [class_method()].
%% @doc extract all of the methods from interfaces.
extract_methods(Ref, Index, Implements) when is_reference(Ref) ->
    AllMethodsDict = lists:foldl(fun({NS, I}, D) ->
        {ok, #class{extends = Extends,
                    extends_ns = ExtendsNS,
                    methods = ClassMethods}} = get(Ref, NS, I),
        NewD = extract_methods(I, Index, ClassMethods, D),
        case Extends of
            undefined ->
                NewD;
            Parent ->
                {ok, #class{methods = ParentMethods}} = get(Ref, ExtendsNS, Parent),
                extract_methods(Parent, Index, ParentMethods, NewD)
        end
    end, [], lists:reverse(Implements)),
    [ V || {_,V} <- AllMethodsDict ].


-type methods_dict() :: [{MethodName :: binary(),
                          {class_name(), class_method()}}].

-spec extract_methods(class_name(), Index::mixed(), [class_name()],
                      methods_dict()) -> methods_dict().
%% @hidden
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


-spec arg_to_text(variable() | var_ref()) -> binary().
%% @hidden
arg_to_text(#variable{name = Name}) -> <<"$", Name/binary>>;
arg_to_text(#var_ref{ref = #variable{name = Name}}) -> <<"$", Name/binary>>.


-spec check_dup([class()]) -> boolean().
%% @hidden
check_dup([]) -> ok;
check_dup([_Interface]) -> ok;
check_dup([Interface|Interfaces]) ->
    case lists:member(Interface, Interfaces) of
        true -> Interface;
        false -> check_dup(Interfaces)
    end.


-spec check_methods(class(), [class_method()]) -> [binary()].
%% @hidden
check_methods(Interface, ClassMethods) ->
    lists:map(fun({IntName, #class_method{name = Name}}) ->
        io_lib:format("~s::~s", [IntName, Name])
    end, check_methods(Interface, ClassMethods, [])).


-spec check_methods([{class_name(), class_method()}], [class_method()],
                    [{class_name(), class_method()}]) ->
      [{class_name(), class_method()}].
%% @hidden
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


-type method_name() :: binary().
-type check_final_methods_return() :: ok |
                                      {error, {class_name(), method_name()}}.

-spec check_final_methods(ephp:classes_id(), [class_method()], namespace(), class_name()) ->
      check_final_methods_return().
%% @doc hidden
check_final_methods(_Ref, [], _ExtendsNS, _Extends) ->
    ok;

check_final_methods(_Ref, _Methods, _ParentNS, undefined) ->
    ok;

check_final_methods(Ref, Methods, ParentNS, ParentClass) ->
    {ok, #class{methods = ParentMethods,
                extends = Extends}} = get(Ref, ParentNS, ParentClass),
    case check_final_methods(Methods, ParentMethods) of
        ok -> check_final_methods(Ref, Methods, ParentNS, Extends);
        {error, MethodError} -> {error, {ParentClass, MethodError}}
    end.


-type check_final_methods_simple_return() :: ok | {error, method_name()}.

-spec check_final_methods([class_method()], [class_method()]) ->
      check_final_methods_simple_return().
%% @doc hidden
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
                                     namespace = NS,
                                     constants = Constants} = PHPInterface) ->
    Classes = ephp_context:get_classes(Ref),
    #class_state{classes = Interfaces} = State = erlang:get(Classes),
    ConstDef = get_extends_consts(Ref, PHPInterface) ++ Constants,
    ActivePHPInterface = PHPInterface#class{file = File, constants = ConstDef},
    case dict:find({NS, Name}, Interfaces) of
        error ->
            NewClasses = dict:store({NS, Name}, ActivePHPInterface, Interfaces),
            NewState = State#class_state{classes = NewClasses},
            erlang:put(Classes, NewState),
            ok;
        {ok, _} ->
            ephp_error:error({error, eredefinedclass, Index, ?E_ERROR, {Name}})
    end.

get_extends_consts(_Ref, #class{extends = undefined}) ->
    [];
get_extends_consts(Ref, #class{name = Name, extends = Extends,
                               extends_ns = ExtendsNS,
                               type = interface, line = Index}) ->
    case get(Ref, ExtendsNS, Extends, true) of
        {ok, #class{type = interface} = Interface} ->
            get_consts(Interface) ++ get_extends_consts(Ref, Interface);
        {ok, #class{}} ->
            ephp_error:error({error, ecannotimpl, Index, ?E_ERROR,
                              {Name, Extends}});
        _ ->
            ephp_error:error({error, enointerface, Index, ?E_ERROR, {Extends}})
    end;
get_extends_consts(Ref, #class{name = Name,
                               extends = Extends,
                               extends_ns = ExtendsNS,
                               line = Index}) ->
    case get(Ref, ExtendsNS, Extends, true) of
        {ok, #class{name = FName, final = true}} ->
            ephp_error:error({error, efinalclass, Index, ?E_ERROR,
                              {FName, Name}});
        {ok, #class{type = interface}} ->
            ephp_error:error({error, ecannotextends, Index, ?E_ERROR,
                              {Name, Extends}});
        {ok, #class{} = Class} ->
            get_consts(Class) ++ get_extends_consts(Ref, Class);
        _ ->
            %% TODO this error should appears in the same line as extends,
            %%      Index is not valid because is pointing to the end
            ephp_error:error({error, eundefclass, Index, ?E_ERROR, {ExtendsNS, Extends}})
    end.

instance(_Ref, LocalCtx, GlobalCtx, ClassNS, ClassName, Line) ->
    case get(LocalCtx, ClassNS, ClassName, true) of
    {ok, #class{} = Class} ->
        {ok, Ctx} = ephp_context:generate_subcontext(LocalCtx, GlobalCtx),
        RegClass = #ephp_object{class = Class, context = Ctx},
        Objects = ephp_context:get_objects(LocalCtx),
        ObjectId = ephp_object:add(Objects, RegClass),
        initialize(Ctx, Class#class.name, Class),
        #obj_ref{pid = Objects, ref = ObjectId};
    {error, enoexist} ->
        ephp_error:error({error, eundefclass, Line, ?E_ERROR,
                          {ClassNS, ClassName}})
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
    case get(Classes, Class#class.namespace, Name) of
        {ok, #class{name = ClassName}} ->
            lists:any(fun(F) -> F() end, [
                fun() -> ClassName =:= InstanceName end,
                fun() -> lists:member(ClassName, Parents) end
            ]);
        {error, _} ->
            false
    end;
instance_of(_Context, #class{name = CName}, CName) ->
    true;
instance_of(_Context, #class{parents = Parents}, CName) ->
    lists:member(CName, Parents);
instance_of(_Context, _Data, _Type) ->
    false.

initialize_class(#class{static_context = Ctx, attrs = Attrs}) ->
    Bulk = lists:foldl(fun
        (#class_attr{type = static, name = Name, init_value = RawVal}, Acc) ->
            Val = ephp_context:solve(Ctx, RawVal),
            [{#variable{name = Name}, Val}|Acc];
        (#class_attr{type = normal}, Acc) ->
            Acc
    end, [], Attrs),
    ephp_context:set_bulk(Ctx, lists:reverse(Bulk)).

initialize(Ctx, _ClassName, #class{attrs=Attrs}) ->
    Bulk = lists:foldl(fun
        (#class_attr{type = normal, access = private, class_name = CName,
                     name = Name, init_value = RawVal}, Acc) ->
            Val = ephp_context:solve(Ctx, RawVal),
            [{#variable{name = {private, Name, CName}}, Val}|Acc];
        (#class_attr{type = normal, name = Name, init_value = RawVal}, Acc) ->
            Val = ephp_context:solve(Ctx, RawVal),
            [{#variable{name=Name}, Val}|Acc];
        (#class_attr{type = static}, Acc) ->
            Acc
    end, [], lists:reverse(Attrs)),
    ephp_context:set_bulk(Ctx, lists:reverse(Bulk)).

get_constructor(Ref, #class{name = Name, methods = Methods,
                            extends_ns = ExtendsNS, extends = Extends}) ->
    MethodName = <<"__construct">>,
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        case lists:keyfind(Name, #class_method.name, Methods) of
        false ->
            case Extends of
                undefined ->
                    undefined;
                Extends ->
                    {ok, Parent} = get(Ref, ExtendsNS, Extends),
                    get_constructor(Ref, Parent)
            end;
        #class_method{} = ClassMethod ->
            ClassMethod
        end;
    #class_method{} = ClassMethod ->
        ClassMethod
    end.

get_clone(Ref, #class{name = Name, methods = Methods,
                      extends_ns = ExtendsNS, extends = Extends}) ->
    MethodName = <<"__clone">>,
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        case lists:keyfind(Name, #class_method.name, Methods) of
        false ->
            case Extends of
                undefined ->
                    undefined;
                Extends ->
                    {ok, Parent} = get(Ref, ExtendsNS, Extends),
                    get_clone(Ref, Parent)
            end;
        #class_method{}=ClassMethod ->
            ClassMethod
        end;
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

get_destructor(Ref, #class{methods = Methods, extends_ns = ExtendsNS, extends = Extends}) ->
    MethodName = <<"__destruct">>,
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        case Extends of
            undefined ->
                undefined;
            Extends ->
                {ok, Parent} = get(Ref, ExtendsNS, Extends),
                get_destructor(Ref, Parent)
        end;
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

get_attribute(#class{attrs = Attrs}, AttributeName) ->
    case lists:keyfind(AttributeName, #class_attr.name, Attrs) of
        false ->
            undefined;
        #class_attr{} = ClassAttr ->
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

get_parent(Context, NS, Name) ->
    Classes = ephp_context:get_classes(Context),
    {ok, #class{extends_ns = ExtendsNS, extends = Extends}} = get(Classes, NS, Name),
    {ExtendsNS, Extends}.

init_static_value(Ref, NS, ClassName, MethodName, VarName, Value) ->
    {ok, #class{methods = Methods}} = {ok, Class} = get(Ref, NS, ClassName),
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
                    set(Ref, NS, ClassName, NewClass),
                    Value
            end;
        false ->
            throw({error, enofunc})
    end.

set_static(Ref, NS, ClassName, MethodName, Vars, Context) ->
    {ok, #class{methods = Methods}} = {ok, Class} = get(Ref, NS, ClassName),
    case lists:keyfind(MethodName, #class_method.name, Methods) of
        #class_method{static = Static} = Method ->
            NewStatic = lists:map(fun({Key, _}) ->
                %% TODO check behaviour when use unset
                NewValue = ephp_vars:get(Vars, #variable{name = Key}, Context),
                {Key, NewValue}
            end, Static),
            NewMethod = Method#class_method{static = NewStatic},
            NewMethods = lists:keyreplace(MethodName,
                                          #class_method.name,
                                          Methods,
                                          NewMethod),
            NewClass = Class#class{methods = NewMethods},
            set(Ref, NS, ClassName, NewClass);
        false when Class#class.extends =/= undefined ->
            set_static(Ref, Class#class.extends_ns, Class#class.extends, MethodName, Vars, Context)
    end,
    ok.

get_consts(#class{constants = Constants}) ->
    Constants.

add_if_no_exists_attrib(#class{} = Class, Names) when is_list(Names) ->
    lists:foldl(fun(Name, C) ->
        add_if_no_exists_attrib(C, Name)
    end, Class, Names);
add_if_no_exists_attrib(#class{attrs = Attrs} = Class, Name) ->
    case get_attribute(Class, Name) of
        undefined ->
            Class#class{
                attrs = Attrs ++ [#class_attr{name = Name}]
            };
        _ ->
            Class
    end.

get_stdclass() ->
    #class{
        name = <<"stdClass">>,
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

register_classes(ClassesRef, Context, Classes) ->
    Consts = ephp_context:get_consts(Context),
    lists:foreach(fun
        (#class{type = normal} = Class) ->
            register_unsafe_class(ClassesRef, Consts, Context, Class);
        (#class{type = interface} = Class) ->
            register_unsafe_interface(ClassesRef, Class)
    end, Classes),
    ok.

register_classes(ClassesRef, Context) ->
    Classes = [
        get_stdclass(),
        get_traversable(),
        get_iterator(),
        get_iterator_aggregate(),
        get_array_access()
    ],
    register_classes(ClassesRef, Context, Classes).

get_unsafe_extends_consts(_Classes, #class{extends = undefined}) ->
    [];
get_unsafe_extends_consts(Classes, #class{extends = Extends,
                                          extends_ns = ExtendsNS,
                                          type = interface}) ->
    {ok, #class{type = interface} = Interface} = get(Classes, ExtendsNS, Extends),
    get_consts(Interface) ++ get_unsafe_extends_consts(Classes, Interface);
get_unsafe_extends_consts(Classes, #class{extends = Extends,
                                          extends_ns = ExtendsNS}) ->
    {ok, #class{} = Class} = get(Classes, ExtendsNS, Extends),
    get_consts(Class) ++ get_unsafe_extends_consts(Classes, Class).

register_unsafe_class(Classes, Consts, GlobalCtx,
                      #class{name = Name, constants = ConstDef0,
                             methods = ClassMethods,
                             namespace = NS,
                             line = Index} = PHPClass) ->
    ConstDef = lists:flatmap(fun({INS, I}) ->
        {ok, #class{type = interface} = Interface} = get(Classes, INS, I),
        get_consts(Interface)
    end, PHPClass#class.implements) ++
         get_unsafe_extends_consts(Classes, PHPClass) ++
         ConstDef0,
    Methods = extract_methods(Classes, Index, PHPClass#class.implements),
    Parents = extract_parents(Classes, NS, PHPClass),
    ok = check_dup(PHPClass#class.implements),
    [] = check_methods(Methods, PHPClass#class.methods),
    ok = check_final_methods(Classes, PHPClass#class.methods,
                             PHPClass#class.extends_ns,
                             PHPClass#class.extends),
    ephp_const:set_bulk(Consts, NS, Name, ConstDef),
    {ok, Ctx} = ephp_context:generate_subcontext(GlobalCtx),
    ActivePHPClass = PHPClass#class{
        static_context = Ctx,
        parents = Parents,
        file = <<"built-in">>,
        methods = [ CM#class_method{class_name = Name,
                                    namespace = NS} || CM <- ClassMethods ] ++
                  get_methods(Classes, PHPClass#class.extends_ns, PHPClass#class.extends),
        attrs = get_attrs(Classes, PHPClass)
    },
    initialize_class(ActivePHPClass),
    set(Classes, NS, Name, ActivePHPClass),
    ok.

register_unsafe_interface(Classes,
                          #class{name = Name,
                                 namespace = NS,
                                 constants = Constants} = PHPInterface) ->
    #class_state{classes = Interfaces} = State = erlang:get(Classes),
    ConstDef = get_unsafe_extends_consts(Classes, PHPInterface) ++ Constants,
    ActivePHPInterface = PHPInterface#class{file = <<"built-in">>,
                                            constants = ConstDef},
    error = dict:find({NS, Name}, Interfaces),
    NewClasses = dict:store({NS, Name}, ActivePHPInterface, Interfaces),
    NewState = State#class_state{classes = NewClasses},
    erlang:put(Classes, NewState),
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


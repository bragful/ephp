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
    get_constructor/1,
    get_destructor/1,
    get_attribute/2,
    get_method/2,
    get_const/2,
    get_const/3,
    get_consts/1,
    get_consts/2,

    class_attr/1,
    class_attr/2,
    class_attr/3,
    class_attr/4,

    init_static_value/5,
    set_static/4,

    instance_of/2,

    register_class/4,
    register_interface/3,
    set_alias/3,
    instance/5,

    get_stdclass/0,
    add_if_no_exists_attrib/2
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, dict:new()),
    ok = register_classes(Ref),
    {ok, Ref}.

destroy(Classes) ->
    erlang:erase(Classes),
    ok.

get(Ref, ClassName) ->
    Classes = erlang:get(Ref),
    case dict:find(ClassName, Classes) of
        {ok, {alias, NewClassName}} ->
            get(Ref, NewClassName);
        {ok, Class} ->
            {ok, Class};
        error ->
            {error, enoexist}
    end.

set(Ref, ClassName, Class) ->
    Classes = erlang:get(Ref),
    NC = dict:store(ClassName, Class, Classes),
    erlang:put(Ref, NC),
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

register_class(Ref, File, GlobalCtx,
               #class{name = Name, constants = ConstDef0,
                      line = Index} = PHPClass) ->
    Classes = erlang:get(Ref),
    {ok, Ctx} = ephp_context:start_link(),
    ephp_context:set_global(Ctx, GlobalCtx),
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
    case check_methods(Methods, PHPClass#class.methods) of
        [] ->
            ok;
        Errors when is_list(Errors) ->
            Params = length(Errors),
            Names = string:join(Errors, ", "),
            ephp_error:error({error, enomethods, Index, ?E_ERROR,
                              {Name, Names, Params}})
    end,
    ActivePHPClass = PHPClass#class{
        static_context = Ctx,
        file = File,
        constants = lists:foldl(fun(#class_const{name = N,value = V}, D) ->
            %% FIXME: check duplicates
            dict:store(N, ephp_context:solve(Ctx, V), D)
        end, dict:new(), ConstDef)
    },
    initialize_class(ActivePHPClass),
    erlang:put(Ref, dict:store(Name, ActivePHPClass, Classes)),
    ok.

extract_methods(Ref, Index, Implements) when is_reference(Ref) ->
    AllMethodsDict = lists:foldl(fun(I, D) ->
        {ok, #class{name = Name,
                    methods = ClassMethods}} = get(Ref, I),
        extract_methods(Name, Index, ClassMethods, D)
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

register_interface(Ref, File, #class{name = Name, line = Index,
                                     constants = Constants} = PHPInterface) ->
    Interfaces = erlang:get(Ref),
    ConstDef = get_extends_consts(Ref, PHPInterface) ++ Constants,
    ActivePHPInterface = PHPInterface#class{file = File, constants = ConstDef},
    case dict:find(Name, Interfaces) of
        error ->
            erlang:put(Ref, dict:store(Name, ActivePHPInterface, Interfaces)),
            ok;
        {ok, _} ->
            ephp_error:error({error, eredefinedclass, Index, ?E_ERROR, {Name}})
    end.

get_extends_consts(_Ref, #class{extends = undefined}) ->
    [];
get_extends_consts(Ref, #class{name = Name, extends = Extends, line = Index}) ->
    case get(Ref, Extends) of
        {ok, #class{type = interface} = Interface} ->
            get_consts(Interface);
        {ok, #class{}} ->
            ephp_error:error({error, ecannotimpl, Index, ?E_ERROR,
                              {Name, Extends}});
        _ ->
            ephp_error:error({error, enointerface, Index, ?E_ERROR, {Extends}})
    end.

instance(Ref, LocalCtx, GlobalCtx, RawClassName, Line) ->
    case get(Ref, RawClassName) of
    {ok, #class{} = Class} ->
        {ok, Ctx} = ephp_context:start_link(),
        ephp_context:set_global(Ctx, GlobalCtx),
        RegClass = #ephp_object{class = Class, context = Ctx},
        Objects = ephp_context:get_objects(LocalCtx),
        ObjectId = ephp_object:add(Objects, RegClass),
        initialize(Ctx, Class),
        #obj_ref{pid = Objects, ref = ObjectId};
    {error, enoexist} ->
        ephp_error:error({error, eundefclass, Line, ?E_ERROR,
                          {RawClassName}})
    end.

-spec instance_of(mixed(), DataType::binary()) -> boolean().

instance_of(#ephp_array{}, <<"array">>) ->
    true;
instance_of(Boolean, <<"bool">>) when is_boolean(Boolean) ->
    true;
instance_of(String, <<"string">>) when is_binary(String) ->
    true;
instance_of(Float, <<"float">>) when is_float(Float) ->
    true;
instance_of(Int, <<"int">>) when is_integer(Int) ->
    true;
% TODO:
% instance_of(Callable, <<"callable">>) ->
%     false;
% TODO:
% instance_of(Self, <<"self">>) ->
%     false;
instance_of(ObjRef, Name) when ?IS_OBJECT(ObjRef) ->
    #ephp_object{class = Class} = ephp_object:get(ObjRef),
    #class{extends = Extends,
           implements = Impl,
           name = ClassName} = Class,
    %% TODO: find another way to implement this in a lazy way (orelse break cover)
    lists:any(fun(F) -> F() end, [
        fun() -> ClassName =:= Name end,
        fun() -> member(Name, Extends) end,
        fun() -> member(Name, Impl) end
    ]);
instance_of(_, _) ->
    false.

member(_, undefined) -> false;
member(Name, List) when is_list(List) -> lists:member(Name, List).

initialize_class(#class{static_context=Ctx, attrs=Attrs}) ->
    lists:foreach(fun
        (#class_attr{type=static, name=Name, init_value=RawVal}) ->
            Val = ephp_context:solve(Ctx, RawVal),
            ephp_context:set(Ctx, #variable{name=Name}, Val);
        (#class_attr{type=normal}) ->
            ignore
    end, Attrs).

initialize(Ctx, #class{attrs=Attrs}) ->
    lists:foreach(fun
        (#class_attr{type=normal, name=Name, init_value=RawVal}) ->
            Val = ephp_context:solve(Ctx, RawVal),
            ephp_context:set(Ctx, #variable{name=Name}, Val);
        (#class_attr{type=static}) ->
            ignore
    end, Attrs).

get_constructor(#class{name=Name, methods=Methods}) ->
    MethodName = <<"__construct">>,
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        case lists:keyfind(Name, #class_method.name, Methods) of
        false ->
            undefined;
        #class_method{}=ClassMethod ->
            ClassMethod
        end;
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

get_destructor(#class{methods=Methods}) ->
    MethodName = <<"__destruct">>,
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        undefined;
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

get_method(#class{methods=Methods,line=Index}, MethodName) ->
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        %% TODO: search "__call" method
        ephp_error:error({error, eundefmethod, Index, ?E_ERROR, {MethodName}});
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

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
    #class_method{static = Static} = Method =
        lists:keyfind(MethodName, #class_method.name, Methods),
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
    set(Ref, ClassName, NewClass),
    ok.

get_const(Ref, ClassName, ConstName) ->
    {ok, Class} = get(Ref, ClassName),
    get_const(Class, ConstName).

get_const(#class{constants = Const, line = Index}, ConstName) ->
    case dict:find(ConstName, Const) of
        {ok, Value} ->
            Value;
        error ->
            ephp_error:error({error, enoconst, Index, ?E_ERROR, {ConstName}})
    end.

get_consts(#class{constants = Constants}) ->
    Constants.

get_consts(Ref, ClassName) ->
    {ok, Class} = get(Ref, ClassName),
    get_consts(Class).

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
        ephp_class_exception:get_class()
    ],
    ClassesDict = lists:foldl(fun(#class{name=Name} = Class, Dict) ->
        dict:store(Name, Class, Dict)
    end, erlang:get(Ref), Classes),
    erlang:put(Ref, ClassesDict),
    ok.

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

    class_attr/1,
    class_attr/2,
    class_attr/3,
    class_attr/4,

    init_static_value/5,
    set_static/4,

    instance_of/2,

    register_class/4,
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
               #class{name=Name,constants=ConstDef}=PHPClass) ->
    Classes = erlang:get(Ref),
    {ok, Ctx} = ephp_context:start_link(),
    ephp_context:set_global(Ctx, GlobalCtx),
    ActivePHPClass = PHPClass#class{
        static_context = Ctx,
        file = File,
        constants = lists:foldl(fun(#class_const{name=N,value=V}, D) ->
            dict:store(N,ephp_context:solve(Ctx, V),D)
        end, dict:new(), ConstDef)
    },
    initialize_class(ActivePHPClass),
    erlang:put(Ref, dict:store(Name, ActivePHPClass, Classes)),
    ok.

instance(Ref, LocalCtx, GlobalCtx, RawClassName, Line) ->
    case get(Ref, RawClassName) of
    {ok, #class{} = Class} ->
        {ok, Ctx} = ephp_context:start_link(),
        ephp_context:set_global(Ctx, GlobalCtx),
        RegClass = #ephp_object{class = Class, context = Ctx},
        Objects = ephp_context:get_objects(LocalCtx),
        ObjectId = ephp_object:add(Objects, RegClass),
        initialize(Ctx, Class),
        RegClass#ephp_object{id = ObjectId, objects = Objects};
    {error, enoexist} ->
        File = ephp_context:get_active_file(LocalCtx),
        ephp_error:error({error, eundefclass, Line, File, ?E_ERROR,
                          {RawClassName}})
    end.

-spec instance_of(mixed(), DataType::binary()) -> boolean().

instance_of(#ephp_array{}, <<"array">>) ->
    true;
instance_of(Boolean, <<"boolean">>) when is_boolean(Boolean) ->
    true;
instance_of(String, <<"string">>) when is_binary(String) ->
    true;
instance_of(Float, <<"float">>) when is_float(Float) ->
    true;
instance_of(Int, <<"integer">>) when is_integer(Int) ->
    true;
% TODO:
% instance_of(Callable, <<"callable">>) ->
%     false;
% TODO:
% instance_of(Self, <<"self">>) ->
%     false;
instance_of(#ephp_object{class = #class{name = Name}}, Name) ->
    true;
instance_of(#ephp_object{class = #class{extends = Extends,
                                         implements = Impl}}, Name) ->
    lists:member(Name, Extends) orelse lists:member(Name, Impl);
instance_of(_, _) ->
    false.

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

get_const(#class{constants=Const}, ConstName) ->
    case dict:find(ConstName, Const) of
        {ok, Value} -> Value;
        error -> ConstName
    end.

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

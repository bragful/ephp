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

    register_class/3,
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
    ok = register_stdclass(Ref),
    {ok, Ref}.

destroy(Classes) ->
    erlang:erase(Classes).

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

register_class(Ref, GlobalCtx, #class{name=Name,constants=ConstDef}=PHPClass) ->
    Classes = erlang:get(Ref),
    {ok, Ctx} = ephp_context:start_link(),
    ephp_context:set_global(Ctx, GlobalCtx),
    ActivePHPClass = PHPClass#class{
        static_context = Ctx,
        constants = lists:foldl(fun(#class_const{name=N,value=V}, D) ->
            dict:store(N,ephp_context:solve(Ctx, V),D)
        end, dict:new(), ConstDef)
    },
    initialize_class(ActivePHPClass),
    erlang:put(Ref, dict:store(Name, ActivePHPClass, Classes)),
    ok.

instance(Ref, LocalCtx, GlobalCtx, RawClassName, Line) ->
    case get(Ref, RawClassName) of
    {ok, #class{name=ClassName}=Class} ->
        {ok, Ctx} = ephp_context:start_link(),
        ephp_context:set_global(Ctx, GlobalCtx),
        % FIXME: looks like the increment is global for all of the instances
        InsCount = Class#class.instance_counter + 1,
        RegClass = #reg_instance{
            id=InsCount,
            class=Class,
            context=Ctx},
        initialize(Ctx, Class),
        set(Ref, ClassName, Class#class{instance_counter=InsCount}),
        RegClass;
    {error, enoexist} ->
        File = ephp_context:get_active_file(LocalCtx),
        ephp_error:error({error, eundefclass, Line, ?E_ERROR,
            {File, RawClassName}})
    end.

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
        ephp_error:error({error, eundefmethod, Index, ?E_ERROR, MethodName});
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

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
        static_context = undefined,
        constants = dict:new(),
        attrs = []
    }.

%% ------------------------------------------------------------------
%% Private functions
%% ------------------------------------------------------------------

register_stdclass(Ref) ->
    Classes = erlang:get(Ref),
    #class{name=Name} = StdClass = get_stdclass(),
    erlang:put(Ref, dict:store(Name, StdClass, Classes)),
    ok.

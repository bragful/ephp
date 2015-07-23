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
    get_attribute/3,
    get_attribute/2,
    get_method/2,
    get_method/3,

    register_class/3,
    instance/4
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, ?DICT:new()),
    {ok, Ref}.

destroy(Classes) ->
    erlang:erase(Classes).

get(Ref, ClassName) ->
    Classes = erlang:get(Ref),
    ?DICT:find(ClassName, Classes).

set(Ref, ClassName, Class) ->
    Classes = erlang:get(Ref),
    NC = ?DICT:store(ClassName, Class, Classes),
    erlang:put(Ref, NC),
    ok.

register_class(Ref, GlobalCtx, #class{name=Name}=PHPClass) ->
    Classes = erlang:get(Ref),
    {ok, Ctx} = ephp_context:start_link(),
    ephp_context:set_global(Ctx, GlobalCtx),
    ActivePHPClass = PHPClass#class{
        static_context = Ctx
    },
    initialize_class(ActivePHPClass),
    erlang:put(Ref, ?DICT:store(Name, ActivePHPClass, Classes)),
    ok.

instance(Ref, GlobalCtx, ClassName, Line) ->
    case get(Ref, ClassName) of
    error ->
        File = ephp_context:get_active_file(GlobalCtx),
        ephp_error:error({error, eundefclass, Line, {File, ClassName}});
    {ok, #class{name=ClassName}=Class} ->
        {ok, Ctx} = ephp_context:start_link(),
        ephp_context:set_global(Ctx, GlobalCtx),
        InsCount = Class#class.instance_counter + 1,
        RegClass = #reg_instance{
            id=InsCount,
            class=Class,
            context=Ctx},
        initialize(Ctx, Class),
        set(Ref, ClassName, Class#class{instance_counter=InsCount}),
        RegClass
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

get_attribute(Ref, ClassName, AttributeName) ->
    {ok, Class} = get(Ref, ClassName),
    get_attribute(Class, AttributeName).

get_attribute(#class{attrs=Attrs}, AttributeName) ->
    case lists:keyfind(AttributeName, #class_attr.name, Attrs) of
    false ->
        undefined;
    #class_attr{}=ClassAttr ->
        ClassAttr
    end.

get_method(Ref, ClassName, MethodName) ->
    {ok, Class} = get(Ref, ClassName),
    get_method(Class, MethodName).

get_method(#class{methods=Methods,line=Index}, MethodName) ->
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        %% TODO: search "__call" method
        ephp_error:error({error, eundefmethod, Index, MethodName});
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

%% ------------------------------------------------------------------
%% Private functions
%% ------------------------------------------------------------------

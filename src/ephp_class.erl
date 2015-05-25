-module(ephp_class).

-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    destroy/1,

    get/2,
    get_constructor/2,
    get_destructor/2,
    get_method/3,

    register_class/2,
    instance/3
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

register_class(Ref, #class{name=Name}=PHPClass) ->
    Classes = erlang:get(Ref),
    erlang:put(Ref, ?DICT:store(Name, PHPClass, Classes)),
    ok.

instance(Ref, GlobalCtx, ClassName) ->
    case get(Ref, ClassName) of
    error ->
        %% TODO: enhance the error handling!
        io:format("~p~n", [ClassName]),
        throw(eundefclass);
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

initialize(Ctx, #class{attrs=Attrs}) ->
    lists:foreach(fun(#class_attr{name=Name, init_value=RawVal}) ->
        Val = ephp_context:solve(Ctx, RawVal), 
        ephp_context:set(Ctx, #variable{name=Name}, Val)
    end, Attrs).

get_constructor(Ref, ClassName) ->
    MethodName = <<"__construct">>,
    {ok, #class{methods=Methods}} = get(Ref, ClassName),
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        undefined;
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

get_destructor(Ref, ClassName) ->
    MethodName = <<"__destruct">>,
    {ok, #class{methods=Methods}} = get(Ref, ClassName),
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        undefined;
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

get_method(Ref, ClassName, MethodName) ->
    {ok, #class{methods=Methods}} = get(Ref, ClassName),
    case lists:keyfind(MethodName, #class_method.name, Methods) of
    false ->
        %% TODO: search "__call" method
        throw(eundefmethod);
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

%% ------------------------------------------------------------------
%% Private functions
%% ------------------------------------------------------------------

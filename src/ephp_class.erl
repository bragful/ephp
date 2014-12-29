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
    get_method/3,

    register_class/2,
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

register_class(Ref, #class{name=Name}=PHPClass) ->
    Classes = erlang:get(Ref),
    erlang:put(Ref, ?DICT:store(Name, PHPClass, Classes)),
    ok.

instance(Ref, GlobalCtx, ClassName, Args) ->
    case get(Ref, ClassName) of
    error ->
        %% TODO: enhance the error handling!
        io:format("~p~n", [{ClassName, Args}]),
        throw(eundefclass);
    {ok, #class{name=ClassName}} ->
        {ok, Ctx} = ephp_context:start_link(),
        ephp_context:set_global(Ctx, GlobalCtx),
        RegClass = #reg_instance{class=ClassName, context=Ctx},
        %% TODO: initialize vars
        %% TODO: run __construct
        RegClass
    end.

get_method(Ref, ClassName, MethodName) ->
    {ok, Class} = get(Ref, ClassName),
    case lists:keyfind(MethodName, 2, Class#class.methods) of
    false ->
        throw(eundefmethod);
    #class_method{}=ClassMethod ->
        ClassMethod
    end.

-module(ephp_class).

-compile([warnings_as_errors]).

-include("ephp.hrl").

-type access() :: public | private | protected.

-record(reg_method, {
    name :: binary(),
    args :: [variable()],
    access :: access()
}).

-type method() :: #reg_method{}.

-record(reg_interface, {
    name :: binary(),
    methods :: [method()]
}).

-type interface() :: #reg_interface{}.

-record(reg_class, {
    name :: binary(),
    % this stores attribs, consts and methods:
    context :: context(),
    extends :: context(),
    implements :: [interface()]
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    destroy/1,

    get/2,

    register_class/3
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

register_class(Ref, GlobalCtx, PHPClass, Extends, Implements) ->
    {ok, Ctx} = ephp_context:start_link(),
    ephp_context:set_global(Ctx, GlobalCtx),
    Classes = erlang:get(Ref),
    RegClass = #reg_class{name=PHPClass, context=Ctx},
    erlang:put(Ref, ?DICT:store(PHPClass, RegClass, Classes)),
    ok.

-module(ephp_const).

-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    get/2,
    set/3,
    destroy/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, ?DICT:new()),
    {ok, Ref}.

get(Ref, Name) ->
    Const = erlang:get(Ref),
    case ?DICT:find(Name, Const) of
        {ok, Value} -> Value;
        error -> Name
    end.

set(Ref, Name, Value) ->
    Const = erlang:get(Ref),
    put(Ref, ?DICT:store(Name, Value, Const)),
    ok.

destroy(Const) ->
    erlang:erase(Const).

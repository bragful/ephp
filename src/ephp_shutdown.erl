-module(ephp_shutdown).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    register_func/2,
    unregister_func/2,

    get_funcs/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, []),
    {ok, Ref}.

register_func(Ref, FuncName) ->
    erlang:put(Ref, erlang:get(Ref) ++ [FuncName]),
    ok.

unregister_func(Ref, FuncName) ->
    erlang:put(Ref, erlang:get(Ref) -- [FuncName]),
    ok.

get_funcs(Ref) ->
    erlang:get(Ref).

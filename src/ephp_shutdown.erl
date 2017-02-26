-module(ephp_shutdown).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    destroy/1,
    register_func/2,
    unregister_func/2,

    get_funcs/1,
    shutdown/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, reference()}.

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, []),
    {ok, Ref}.

-spec destroy(ephp:shutdown_id()) -> ok.

destroy(Ref) ->
    erlang:erase(Ref),
    ok.

-spec register_func(reference(), function_name()) -> ok.

register_func(Ref, FuncName) ->
    erlang:put(Ref, erlang:get(Ref) ++ [FuncName]),
    ok.

-spec unregister_func(reference(), function_name()) -> ok.

unregister_func(Ref, FuncName) ->
    erlang:put(Ref, erlang:get(Ref) -- [FuncName]),
    ok.

-spec get_funcs(reference()) -> [function_name()].

get_funcs(Ref) ->
    erlang:get(Ref).

-spec shutdown(context()) -> undefined.

shutdown(Context) ->
    Result = lists:foldl(fun
        (FuncName, false) ->
            Shutdown = #call{name = FuncName},
            ephp_interpr:run(Context, #eval{statements=[Shutdown]});
        (_, Break) ->
            Break
    end, false, ephp_context:get_shutdown_funcs(Context)),
    if Result =:= false ->
        ephp_array:fold(fun(K,V,Acc) ->
            ephp_lib_vars:unset(Context, undefined, {#variable{name=K},V}),
            Acc
        end, undefined, ephp_context:get(Context, #variable{name = <<"GLOBALS">>}));
    true ->
        undefined
    end.

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

-spec shutdown(context()) -> null.

shutdown(Context) ->
    Result = lists:foldl(fun
        (FuncName, false) ->
            Shutdown = #call{name = FuncName},
            ephp_interpr:run(Context, #eval{statements=[Shutdown]});
        (_, Break) ->
            Break
    end, false, ephp_context:get_shutdown_funcs(Context)),
    if Result =:= false ->
        ?DICT:fold(fun(K,V,Acc) ->
            ephp_func_vars:unset(Context, undefined, {#variable{name=K},V}),
            Acc
        end, null, ephp_context:get(Context, #variable{name = <<"GLOBALS">>}));
    true ->
        null
    end.

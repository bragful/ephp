-module(ephp_shutdown).
-author('manuel@altenwald.com').

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    destroy/1,
    register_func/3,
    unregister_func/3,

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

-spec register_func(reference(), namespace(), callable()) -> ok.

register_func(Ref, NS, FuncName) ->
    erlang:put(Ref, erlang:get(Ref) ++ [{NS, FuncName}]),
    ok.

-spec unregister_func(reference(), namespace(), callable()) -> ok.

unregister_func(Ref, NS, FuncName) ->
    erlang:put(Ref, erlang:get(Ref) -- [{NS, FuncName}]),
    ok.

-spec get_funcs(reference()) -> [callable()].

get_funcs(Ref) ->
    erlang:get(Ref).

-spec shutdown(context()) -> undefined.

shutdown(Context) ->
    Line = {{line,0}, {column,0}},
    Result = lists:foldl(fun
        ({NS, FuncName}, false) ->
            Shutdown = #call{name = FuncName, namespace = NS, line = Line},
            Eval = #eval{statements = [Shutdown], line = Line},
            ephp_interpr:run(Context, Eval);
        (_, Break) ->
            Break
    end, false, ephp_context:get_shutdown_funcs(Context)),
    if Result =:= false ->
        ephp_object:remove_all(Context, ephp_context:get_objects(Context)),
        ephp_vars:destroy(Context, ephp_context:get_vars(Context));
    true ->
        undefined
    end.

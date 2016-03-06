-module(ephp_const).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    get/2,
    get/4,
    set/3,
    set_bulk/2,
    destroy/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    Init = [
        {<<"__FILE__">>, <<>>}
    ],
    Consts = lists:foldl(fun({K,V},C) ->
        dict:store(K,V,C)
    end, dict:new(), Init),
    erlang:put(Ref, Consts),
    [ set_bulk(Ref, Module:init_consts()) || Module <- ?CONST_MODULES ],
    {ok, Ref}.

get(Ref, Name) ->
    get(Ref, Name, undefined, undefined).

get(Ref, Name, Line, Context) ->
    Const = erlang:get(Ref),
    case dict:find(Name, Const) of
        {ok, Value} ->
            Value;
        error when Context =:= undefined ->
            Name;
        error ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefconst, Line, ?E_NOTICE, {File, Name}}),
            Name
    end.

set_bulk(Ref, Values) ->
    erlang:put(Ref, lists:foldl(fun({Name, Value}, Const) ->
        dict:store(Name, Value, Const)
    end, erlang:get(Ref), Values)).

set(Ref, Name, Value) ->
    Const = erlang:get(Ref),
    erlang:put(Ref, dict:store(Name, Value, Const)),
    ok.

destroy(Const) ->
    erlang:erase(Const).

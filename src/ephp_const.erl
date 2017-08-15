-module(ephp_const).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    get/4,
    get/5,
    set/3,
    set/4,
    set_bulk/2,
    set_bulk/3,
    destroy/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    Init = [
        {{undefined, <<"__FILE__">>}, <<>>},
        {{undefined, <<"__CLASS__">>}, <<>>},
        {{undefined, <<"__METHOD__">>}, <<>>},
        {{undefined, <<"__FUNCTION__">>}, <<>>}
    ],
    Consts = lists:foldl(fun({K,V},C) ->
        dict:store(K,V,C)
    end, dict:new(), Init),
    erlang:put(Ref, Consts),
    Modules = ephp_config:get(modules, []),
    [ set_bulk(Ref, Module:init_const()) || Module <- Modules ],
    {ok, Ref}.

get(Ref, Name, Line, Context) ->
    get(Ref, undefined, Name, Line, Context).

get(Ref, ClassName, Name, Line, Context) ->
    Const = erlang:get(Ref),
    case dict:find({ClassName, Name}, Const) of
        {ok, Value} ->
            Value;
        error when Line =/= false andalso ClassName =:= undefined ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefconst, Line, File, ?E_NOTICE, {Name}}),
            Name;
        error when Line =/= false ->
            ephp_error:error({error, enoconst, Line, ?E_ERROR, {Name}});
        error ->
            false
    end.

set_bulk(Ref, Values) ->
    set_bulk(Ref, undefined, Values).

set_bulk(Ref, ClassName, Values) ->
    erlang:put(Ref, lists:foldl(fun({Name, Value}, Const) ->
        dict:store({ClassName, Name}, Value, Const)
    end, erlang:get(Ref), Values)).

set(Ref, Name, Value) ->
    set(Ref, undefined, Name, Value).

set(Ref, ClassName, Name, Value) ->
    Const = erlang:get(Ref),
    erlang:put(Ref, dict:store({ClassName, Name}, Value, Const)),
    ok.

destroy(Const) ->
    erlang:erase(Const),
    ok.

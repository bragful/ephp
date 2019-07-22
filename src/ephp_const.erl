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
    get/6,
    set/3,
    set/4,
    set/5,
    set_bulk/2,
    set_bulk/3,
    set_bulk/4,
    destroy/1,
    special_consts/0
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

special_consts() -> [
    <<"__FILE__">>,
    <<"__CLASS__">>,
    <<"__METHOD__">>,
    <<"__FUNCTION__">>,
    <<"__NAMESPACE__">>
].

start_link() ->
    Ref = make_ref(),
    Init = [ {{[], undefined, N}, <<>>} || N <- special_consts() ],
    Consts = lists:foldl(fun({K, V}, C) ->
        dict:store(K, V, C)
    end, dict:new(), Init),
    erlang:put(Ref, Consts),
    Modules = ephp_config:get(modules, []),
    [ set_bulk(Ref, Module:init_const()) || Module <- Modules ],
    {ok, Ref}.

get(Ref, Name, Line, Context) ->
    get(Ref, undefined, Name, Line, Context).

get(Ref, ClassName, Name, Line, Context) ->
    get(Ref, [], ClassName, Name, Line, Context).

get(Ref, NS, ClassName, Name, Line, Context) ->
    Const = erlang:get(Ref),
    case dict:find({NS, ClassName, Name}, Const) of
        {ok, Value} ->
            Value;
        error when Line =/= false andalso ClassName =:= undefined ->
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context,
                {error, eundefconst, Line, File, ?E_NOTICE, {NS, Name}}),
            Name;
        error when Line =/= false ->
            ephp_error:error({error, enoconst, Line, ?E_ERROR, {NS, Name}});
        error ->
            false
    end.

set_bulk(Ref, Values) ->
    set_bulk(Ref, undefined, Values).

set_bulk(Ref, ClassName, Values) ->
    set_bulk(Ref, [], ClassName, Values).

set_bulk(Ref, NameSpace, ClassName, Values) ->
    erlang:put(Ref, lists:foldl(fun({Name, Value}, Const) ->
        dict:store({NameSpace, ClassName, Name}, Value, Const)
    end, erlang:get(Ref), Values)).

set(Ref, Name, Value) ->
    set(Ref, undefined, Name, Value).

set(Ref, ClassName, Name, Value) ->
    set(Ref, [], ClassName, Name, Value).

set(Ref, NameSpace, ClassName, Name, Value) ->
    Const = erlang:get(Ref),
    erlang:put(Ref, dict:store({NameSpace, ClassName, Name}, Value, Const)),
    ok.

destroy(Const) ->
    erlang:erase(Const),
    ok.

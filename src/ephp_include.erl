-module(ephp_include).

-compile([warnings_as_errors]).

-include("ephp.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    load_once/2,
    load/2,
    destroy/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, ?DICT:new()),
    {ok, Ref}.

load_once(Ref, Name) ->
    Inc = erlang:get(Ref),
    case ?DICT:find(Name, Inc) of
    {ok, Value} ->
        Value;
    error ->
        try
            {ok, Content} = file:read_file(Name),
            Value = ephp_parser:parse(Content),
            erlang:put(Ref, ?DICT:store(Name, Value, Inc)),
            Value
        catch
            _:Reason -> {error, Reason}
        end
    end.

load(Ref, Name) ->
    Inc = erlang:get(Ref),
    case ?DICT:find(Name, Inc) of
    {ok, _Value} ->
        {error, duplicated};
    error ->
        try
            {ok, Content} = file:read_file(Name),
            Value = ephp_parser:parse(Content),
            erlang:put(Ref, ?DICT:store(Name, Value, Inc)),
            Value
        catch
            _:Reason -> {error, Reason}
        end
    end.

destroy(Inc) ->
    erlang:erase(Inc).

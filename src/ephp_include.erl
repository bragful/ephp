-module(ephp_include).
-author('manuel@altenwald.com').
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
    {ok, _Value} ->
        {return, true};
    error ->
        case file:read_file(Name) of
        {error, enoent} ->
            {error, enoent};
        {ok, Content} -> 
            Value = ephp_parser:parse(Content),
            erlang:put(Ref, ?DICT:store(Name, Value, Inc)),
            Value
        end
    end.

load(Ref, Name) ->
    Inc = erlang:get(Ref),
    case ?DICT:find(Name, Inc) of
    {ok, Value} ->
        Value;
    error ->
        case file:read_file(Name) of
        {error, enoent} ->
            {error, enoent};
        {ok, Content} -> 
            Value = ephp_parser:parse(Content),
            erlang:put(Ref, ?DICT:store(Name, Value, Inc)),
            Value
        end
    end.

destroy(Inc) ->
    erlang:erase(Inc).

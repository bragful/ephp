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
    {ok, _Value} ->
        {return, true};
    error ->
        {ok, Content} = file:read_file(Name),
        Value = ephp_parser:parse(Content),
        erlang:put(Ref, ?DICT:store(Name, Value, Inc)),
        Value
    end.

load(Ref, Name) ->
    Inc = erlang:get(Ref),
    case ?DICT:find(Name, Inc) of
    {ok, Value} ->
        Value;
    error ->
        {ok, Content} = file:read_file(Name),
        Value = ephp_parser:parse(Content),
        erlang:put(Ref, ?DICT:store(Name, Value, Inc)),
        Value
    end.

destroy(Inc) ->
    erlang:erase(Inc).

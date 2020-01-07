-module(ephp_include).
-author('manuel@altenwald.com').

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
    erlang:put(Ref, dict:new()),
    {ok, Ref}.

load_once(Ref, Name) ->
    Inc = erlang:get(Ref),
    case dict:find(Name, Inc) of
    {ok, _Value} ->
        {return, true};
    error ->
        case find_file(Name) of
        {error, enoent} ->
            {error, enoent};
        {ok, File, Content} ->
            FullPath = filename:join([ephp_stream:get_initial_path(), File]),
            Value = ephp_parser:parse(FullPath, Content),
            erlang:put(Ref, dict:store(Name, Value, Inc)),
            Value
        end
    end.

load(Ref, Name) ->
    Inc = erlang:get(Ref),
    case dict:find(Name, Inc) of
    {ok, Value} ->
        Value;
    error ->
        case find_file(Name) of
        {error, enoent} ->
            {error, enoent};
        {ok, File, Content} ->
            FullPath = filename:join([ephp_stream:get_initial_path(), File]),
            Value = ephp_parser:parse(FullPath, Content),
            erlang:put(Ref, dict:store(Name, Value, Inc)),
            Value
        end
    end.

destroy(Inc) ->
    erlang:erase(Inc),
    ok.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

get_paths() ->
    IncludePath = ephp_config:get(<<"include_path">>, <<".:">>),
    binary:split(IncludePath, ?PATH_SEP, [global]).

find_file(Name) ->
    lists:foldl(fun
        (<<>>, FullPath) ->
            FullPath;
        (Path, {error, enoent}) ->
            FullPath = filename:join(Path, Name),
            case ephp_stream:read_file(FullPath) of
                {ok, Content} -> {ok, FullPath, Content};
                Else -> Else
            end;
        (_, FullPath) ->
            FullPath
    end, {error, enoent}, get_paths()).

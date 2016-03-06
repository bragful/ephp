-module(ephp_data).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    new/0,
    put/2,
    put/3,
    get/2,
    erase/2,
    destroy/1
]).

-include("ephp.hrl").

-record(ephp_data_session, {
    data = #{} :: ephp_data(),
    index = 0 :: non_neg_integer()
}).

new() ->
    Ref = erlang:make_ref(),
    erlang:put(Ref, #ephp_data_session{}),
    {ok, Ref}.

put(Ref, Value) ->
    #ephp_data_session{index=Index, data=OldData}=DataSession=erlang:get(Ref),
    NewIndex = Index+1,
    NewData = maps:put(Index, Value, OldData),
    {ok, Index, #ephp_data_session{index=NewIndex, data=NewData}}.

put(Ref, Index, Value) ->
    #ephp_data_session{data=OldData}=DataSession=erlang:get(Ref),
    NewData = maps:put(Index, Value, OldData),
    erlang:put(Ref, #ephp_data_session{data=NewData}),
    {ok, Index}.

ref(Ref, Index) ->
    #ephp_data_session{data=Data}=DataSession=erlang:get(Ref),
    case maps:find(Index, Data) of
        error ->
            {error, enoexist};
        {ok, #ephp_data{count=N}=Value} ->
            NewValue = Value#ephp_data{count=N+1},
            NewData = maps:put(Index, NewValue, Data),
            erlang:put(Ref, DataSession#ephp_data_session{data=NewData}),
            ok
    end.

get(Ref, Indexes) when is_list(Indexes) ->
    #ephp_data_session{data=Data} = erlang:get(Ref),
    Values = lists:map(fun(Index) ->
        case maps:find(Index, Data) of
            error -> null;
            {ok, #ephp_data{data=Value}} -> Value
        end
    end, Indexes),
    {ok, Values};

get(Ref, Index) ->
    #ephp_data_session{data=Data} = erlang:get(Ref),
    case maps:find(Index, Data) of
        error -> {error, enoexist};
        {ok, #ephp_data{data=Value}} -> {ok, Value}
    end.

erase(Ref, Index) ->
    #ephp_data_session{data=Data}=DataSession=erlang:get(Ref),
    NewData = case maps:find(Index, Data) of
        error ->
            Data;
        {ok, #ephp_data{count=1}} ->
            maps:remove(Index, Data);
        {ok, #ephp_data{count=N}} ->
            NewValue#ephp_data{count=N-1},
            maps:put(Index, NewValue, Data)
    end,
    NewDataSession = DataSession#ephp_data_session{data=NewData},
    erlang:put(Ref, NewDataSession),
    ok.

destroy(Ref) ->
    erlang:erase(Ref).

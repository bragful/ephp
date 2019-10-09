-module(ephp_tracer).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-include("ephp.hrl").

-export([
    start_link/0,
    start_link/1,
    stop/0,
    calls/1,
    erase/1
]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

start_link() ->
    Output = fun(#stack_trace{file = File, line = Line,
                              args = Args} = StackTrace) ->
        io:format("=> ~s:~b\n\tName => ~s\n\tArgs => ~p~n",
                  [File, Line, key_name(StackTrace), Args])
    end,
    start_link(Output).

start_link(Output) ->
    Args = [Output],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:stop(?MODULE).

calls(Name) ->
    gen_server:cast(?MODULE, {calls, Name}).

erase(Name) ->
    gen_server:cast(?MODULE, {erase, Name}).

init([OutputFun]) ->
    {ok, #{filters => [],
           output => OutputFun}}.

handle_cast(#stack_trace{} = StackTrace, #{filters := Filters} = State) ->
    Options = [
        {function, key_name(StackTrace)},
        {file, filename:absname(StackTrace#stack_trace.file)}
    ],
    Check = fun(Filter) -> lists:member(Filter, Options) end,
    case lists:any(Check, Filters) of
        true ->
            #{output := OutputFun} = State,
            OutputFun(StackTrace);
        false ->
            ok
    end,
    {noreply, State};

handle_cast({erase, Name}, #{filters := Filters} = State) ->
    {noreply, State#{filters => Filters -- [Name]}};

handle_cast({calls, Name}, #{filters := Filters} = State) ->
    case lists:member(Name, Filters) of
        true -> {noreply, State};
        false -> {noreply, State#{filters => [Name|Filters]}}
    end.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

key_name(#stack_trace{type = undefined, function = Fun}) ->
    ephp_data:to_bin(Fun);
key_name(#stack_trace{type = <<"::">>, class = Class, function = Fun}) ->
    <<Class/binary, "::", (ephp_data:to_bin(Fun))/binary>>;
key_name(#stack_trace{type = <<"->">>, class = Class, function = Fun}) ->
    <<Class/binary, "->", (ephp_data:to_bin(Fun))/binary>>.

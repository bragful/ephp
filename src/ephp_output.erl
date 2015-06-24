-module(ephp_output).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-type flush_handler() ::
    stdout | {io, io:device()} | function().

-record(state, {
    output = <<>> :: binary(),
    flush = true :: boolean(),
    flush_handler = stdout :: flush_handler()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    start_link/1,
    start_link/2,
    get/1,
    push/2,
    pop/1,
    size/1,
    flush/1,
    set_flush/2,
    destroy/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    start_link(true, stdout).

start_link(Flush) ->
    start_link(Flush, stdout).

start_link(Flush, FlushHandler) ->
    Ref = make_ref(),
    erlang:put(Ref, #state{
        flush = Flush,
        flush_handler = FlushHandler
    }),
    {ok, Ref}.

pop(Ref) ->
    #state{output=Output} = State = erlang:get(Ref),
    erlang:put(Ref, State#state{output = <<>>}),
    Output.

push(Ref, Text) ->
    case erlang:get(Ref) of
    #state{flush=true, flush_handler=FH} ->
        flush_handler(Text, FH);
    #state{flush=false, output=Output}=State ->
        erlang:put(Ref, State#state{output = <<Output/binary, Text/binary>>})
    end,
    ok.

get(Ref) ->
    #state{output=Output} = erlang:get(Ref),
    Output.

set_flush(Ref, Flush) ->
    State = erlang:get(Ref),
    erlang:put(Ref, State#state{flush = Flush}),
    ok.

size(Ref) ->
    #state{output=Output} = erlang:get(Ref),
    byte_size(Output).

flush(Ref) ->
    #state{output=Output, flush_handler=FH} = State = erlang:get(Ref),
    flush_handler(Output, FH),
    erlang:put(Ref, State#state{output = <<>>}),
    ok.

destroy(Ref) ->
    #state{output=Output, flush_handler=FH} = erlang:get(Ref),
    flush_handler(Output, FH),
    erlang:erase(Ref).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

flush_handler(Text, stdout) ->
    io:fwrite("~s", [Text]),
    ok;

flush_handler(Text, {io, FH}) ->
    io:fwrite(FH, "~s", [Text]),
    ok;
%% TODO: add possibility to use a PHP function.
flush_handler(Text, FH) when is_function(FH) ->
    FH(Text),
    ok.

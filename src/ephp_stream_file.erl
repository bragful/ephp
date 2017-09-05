-module(ephp_stream_file).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include_lib("kernel/include/file.hrl").
-include("ephp.hrl").

-behaviour(ephp_stream).

-export([
    open/2,
    close/1,
    read/2,
    write/3
]).

-spec open(ephp_stream:uri(), ephp_stream:options()) ->
      {ok, #file_descriptor{}} | {error, reason()}.
%% @doc opens a file in the filesystem.
open(File, Options) ->
    {Truncate, Opts} = lists:partition(fun(X) -> X =:= truncate end, Options),
    case {file:open(File, [raw, binary|Opts]), Truncate} of
        {{ok, IO}, [truncate]} ->
            file:truncate(IO),
            {ok, IO};
        {OpenReturn, _} ->
            OpenReturn
    end.


-spec close(#file_descriptor{}) -> ok | {error, reason()}.
%% @doc closes a file.
close(PID) ->
    file:close(PID).


-spec read(#file_descriptor{}, ephp_stream:options()) ->
      {ok, binary()} | eof | {error, reason()}.
%% @doc reads data from the file.
read(PID, Options) ->
    {size, Size} = lists:keyfind(size, 1, Options),
    file:read(PID, Size).


-spec write(#file_descriptor{}, binary(), ephp_stream:options()) ->
      ok | {error, reason()}.
%% @doc writes data to a file.
write(PID, Data, _Options) ->
    file:write(PID, Data).

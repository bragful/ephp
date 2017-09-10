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
    write/3,
    position/2,
    is_eof/1
]).

-spec open(ephp_stream:uri(), ephp_stream:options()) ->
      {ok, file:fd()} | {error, reason()}.
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


-spec close(file:fd()) -> ok | {error, reason()}.
%% @doc closes a file.
close(PID) ->
    file:close(PID).


-spec read(file:fd(), ephp_stream:options()) ->
      {ok, binary()} | eof | {error, reason()}.
%% @doc reads data from the file.
read(PID, Options) ->
    {size, Size} = lists:keyfind(size, 1, Options),
    file:read(PID, Size).


-spec write(file:fd(), binary(), ephp_stream:options()) ->
      ok | {error, reason()}.
%% @doc writes data to a file.
write(PID, Data, _Options) ->
    file:write(PID, Data).


-spec position(file:fd(), file:location()) -> ok | {error, reason()}.
%% @doc moves the cursor to the specified position inside of the file.
position(PID, Location) ->
    case file:position(PID, Location) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.


-spec is_eof(file:fd()) -> boolean() | {error, reason()}.
%% @doc returns true if EOF is achieved by the file cursor or false otherwise.
is_eof(FD) ->
    %% FIXME it should to be a much better way to do this... right?
    {ok, Pos} = file:position(FD, cur),
    {ok, Size} = file:position(FD, eof),
    {ok, Pos} = file:position(FD, Pos),
    Pos =:= Size.

-module(ephp_stream_file).

-author('manuel@altenwald.com').

-include_lib("kernel/include/file.hrl").

-include("ephp.hrl").

-behaviour(ephp_stream).

-export([open/2, close/1, read/2, read_file/1, write/3, position/2, is_eof/1,
         file_exists/1, is_dir/1, is_readable/1, wildcard/1]).

-spec open(ephp_stream:uri(), ephp_stream:options()) ->
              {ok, file:fd()} | {error, reason()}.
%% @doc opens a file in the filesystem.
open(URL, Options) ->
    {Truncate, Opts} = lists:partition(fun(X) -> X =:= truncate end, Options),
    FullURL = filename:join([ephp_stream:get_initial_path(), URL]),
    case {file:open(FullURL, [raw, binary | Opts]), Truncate} of
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

-spec read(file:fd(), ephp_stream:options()) -> {ok, binary()} | eof | {error, reason()}.
%% @doc reads data from the file.
read(PID, Options) ->
    {size, Size} = lists:keyfind(size, 1, Options),
    file:read(PID, Size).

-spec read_file(ephp_stream:uri()) -> {ok, binary()} | {error, reason()}.
%% @doc read the whole file from filesystem.
read_file(URL) ->
    FullURL = filename:join([ephp_stream:get_initial_path(), URL]),
    file:read_file(FullURL).

-spec write(file:fd(), binary(), ephp_stream:options()) -> ok | {error, reason()}.
%% @doc writes data to a file.
write(PID, Data, _Options) ->
    file:write(PID, Data).

-spec position(file:fd(), file:location()) -> ok | {error, reason()}.
%% @doc moves the cursor to the specified position inside of the file.
position(PID, Location) ->
    case file:position(PID, Location) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec is_eof(file:fd()) -> boolean() | {error, reason()}.
%% @doc returns true if EOF is achieved by the file cursor or false otherwise.
is_eof(FD) ->
    %% FIXME it should to be a much better way to do this... right?
    {ok, Pos} = file:position(FD, cur),
    {ok, Size} = file:position(FD, eof),
    {ok, Pos} = file:position(FD, Pos),
    Pos =:= Size.

-spec file_exists(ephp_stream:uri()) -> boolean().
%% @doc returns true if the URI exists, otherwise false.
file_exists(URL) ->
    FullURL = filename:join([ephp_stream:get_initial_path(), URL]),
    filelib:is_regular(FullURL).

-spec is_dir(ephp_stream:uri()) -> boolean().
%% @doc returns true if the URL is a directory, otherwise false.
is_dir(URL) ->
    FullURL = filename:join([ephp_stream:get_initial_path(), URL]),
    filelib:is_dir(FullURL).

-spec is_readable(ephp_stream:uri()) -> boolean().
%% @doc returns true if the URL is readable/accesible, otherwise false.
is_readable(URL) ->
    FullURL = filename:join([ephp_stream:get_initial_path(), URL]),
    case file:read_file_info(FullURL) of
        {ok, #file_info{access = Access}} when Access =:= read orelse Access =:= read_write ->
            true;
        _ ->
            false
    end.

-spec wildcard(ephp_stream:pattern()) -> [ephp_stream:uri()].
%% @doc returns the list of URIs for a given pattern or an empty list.
wildcard(Pattern) ->
    BasePath = ephp_stream:get_initial_path(),
    FullPattern = filename:join(BasePath, Pattern),
    Files = filelib:wildcard(binary_to_list(FullPattern)),
    Clean =
        case Pattern of
            "/" ++ _ ->
                fun list_to_binary/1;
            _ ->
                BaseSize = byte_size(BasePath),
                fun(File) ->
                   BinFile = list_to_binary(File),
                   case BinFile of
                       <<BasePath:BaseSize/binary, "/", RelFile/binary>> ->
                           RelFile;
                       <<BasePath:BaseSize/binary, RelFile/binary>> ->
                           RelFile
                   end
                end
        end,
    [Clean(File) || File <- Files].

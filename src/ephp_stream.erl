-module(ephp_stream).
-author('manuel@altenwald.com').

-include_lib("kernel/include/file.hrl").
-include("ephp.hrl").

-export([
    start_link/0,
    set_initial_path/1,
    get_initial_path/0,
    list_streams/0,
    parse_uri/1,
    get_res_id/1,
    open/2,
    close/1,
    read/2,
    read_file/1,
    write/3,
    position/2,
    is_eof/1,
    file_exists/1,
    is_dir/1,
    is_readable/1,
    wildcard/1
]).

-type uri() :: binary().
-type pattern() :: binary().
-type option() :: atom() | {atom(), boolean() | integer() | binary()}.
-type options() :: [option()].

-type stream() :: binary(). %% file, http, ftp, ...

-export_type([uri/0, option/0, options/0, stream/0]).

-type stream_resource() :: any().

-callback open(uri(), options()) -> {ok, stream_resource()} | {error, reason()}.
-callback close(stream_resource()) -> ok | {error, reason()}.
-callback read(stream_resource(), options()) -> {ok, binary()} | eof | {error, reason()}.
-callback write(stream_resource(), binary(), options()) -> ok | {error, reason()}.
-callback position(stream_resource(), file:location()) -> ok | {error, reason()}.
-callback is_eof(stream_resource()) -> boolean() | {error, reason()}.
-callback read_file(stream_resource()) -> {ok, binary()} | {error, reason()}.

-callback file_exists(uri()) -> boolean().
-callback is_dir(uri()) -> boolean().
-callback is_readable(uri()) -> boolean().
-callback wildcard(pattern()) -> [uri()].

-optional_callbacks([file_exists/1, is_dir/1, is_readable/1, wildcard/1]).

-spec list_streams() -> [binary()].
%% @doc get a list of streams loaded in binary format.
list_streams() ->
    AllApps = [ App || {App, _, _} <- application:loaded_applications() ],
    lists:flatmap(fun(App) ->
        {ok, Modules} = application:get_key(App, modules),
        lists:filtermap(fun(Module) ->
            case atom_to_binary(Module, utf8) of
                <<"ephp_stream_", Stream/binary>> -> {true, Stream};
                _ -> false
            end
        end, Modules)
    end, AllApps).


-spec parse_uri(binary()) -> {stream(), uri()}.
%% @doc parse the URI to separate in stream and the rest of the URI.
parse_uri(URL) ->
    case binary:split(URL, <<"://">>) of
        [URL] -> {<<"file">>, URL};
        [<<"file">>, URI] -> {<<"file">>, URI};
        [Schema, _URI] -> {Schema, URL}
    end.


-spec get_mod_and_url(uri()) -> {module(), uri()}.
%% @hidden
get_mod_and_url(URL) ->
    {Schema, URI} = parse_uri(URL),
    Prefix = ephp_config:get(<<"context.prefix_module">>, <<"ephp_stream_">>),
    try
        Module = binary_to_existing_atom(<<Prefix/binary, Schema/binary>>, utf8),
        Module:module_info(),
        case erlang:function_exported(Module, open, 2) of
            true ->
                {Module, URI};
            false ->
                {binary_to_existing_atom(<<Prefix/binary, "file">>, utf8), URL}
        end
    catch
        error:undef ->
            {binary_to_existing_atom(<<Prefix/binary, "file">>, utf8), URL};
        error:badarg ->
            {binary_to_existing_atom(<<Prefix/binary, "file">>, utf8), URL}
    end.


-spec start_link() -> ok.
%% @doc initilize the stream subsystem.
start_link() ->
    erlang:put(resource_next_id, 1),
    ok.


-spec set_initial_path(Path :: uri()) -> ok.
%% @doc set the initial PATH for this process.
set_initial_path(Path) ->
    erlang:put(initial_path, Path).


-spec get_initial_path() -> binary().
%% @doc get the initial PATH for this process.
get_initial_path() ->
    erlang:get(initial_path).


-spec get_res_id(resource()) -> integer().
%% @doc obtains the resource ID given a resource as param.
get_res_id(#resource{id = ID}) ->
    ID.


-spec open(uri(), options()) -> {ok, resource()} | {error, reason()}.
%% @doc open a stream given the URI and options.
open(URL, Options) ->
    {StreamMod, URIorURL} = get_mod_and_url(URL),
    case StreamMod:open(URIorURL, Options) of
        {ok, PID} ->
            ID = get_last_id(),
            {ok, #resource{id = ID, pid = PID, module = StreamMod}};
        {error, Error} ->
            {error, Error}
    end.


-spec close(resource()) -> ok | {error, reason()}.
%% @doc close a stream given the schema and the resource previously opened.
close(#resource{module = Module, pid = PID}) ->
    Module:close(PID).


-spec read(resource(), options()) -> {ok, binary()} | eof | {error, reason()}.
%% @doc request a read to the stream implementation.
read(#resource{module = Module, pid = PID}, Options) ->
    Module:read(PID, Options).


-spec write(resource(), binary(), options()) -> ok | {error, reason()}.
%% @doc request a write to the stream implementation.
write(#resource{module = Module, pid = PID}, Data, Options) ->
    Module:write(PID, Data, Options).


-spec get_last_id() -> pos_integer().
%% @doc retrieves the last ID for consistency in the resource ID.
get_last_id() ->
    ID = erlang:get(resource_next_id),
    erlang:put(resource_next_id, ID + 1),
    ID.


-spec position(resource(), file:location()) -> ok | {error, reason()}.
%% @doc moves the cursor for the stream to the specified location.
position(#resource{module = Module, pid = PID}, Location) ->
    Module:position(PID, Location).


-spec is_eof(resource()) -> boolean() | {error, reason()}.
%% @doc returns true if EOF is achieved by the file cursor or false otherwise.
is_eof(#resource{module = Module, pid = PID}) ->
    Module:is_eof(PID).


-spec read_file(uri()) -> {ok, binary()} | {error, reason()}.
%% @doc read the whole content via URI and return it.
read_file(URL) ->
    {StreamMod, URIorURL} = get_mod_and_url(URL),
    StreamMod:read_file(URIorURL).


-spec file_exists(uri()) -> boolean().
%% @doc returns true if the URI exists, otherwise false.
file_exists(URL) ->
    {StreamMod, URIorURL} = get_mod_and_url(URL),
    case erlang:function_exported(StreamMod, file_exists, 1) of
        true -> StreamMod:file_exists(URIorURL);
        false -> false
    end.


-spec is_dir(uri()) -> boolean().
%% @doc returns true if the URL is a directory, otherwise false.
is_dir(URL) ->
    {StreamMod, URIorURL} = get_mod_and_url(URL),
    case erlang:function_exported(StreamMod, is_dir, 1) of
        true -> StreamMod:is_dir(URIorURL);
        false -> false
    end.


-spec is_readable(uri()) -> boolean().
%% @doc returns true if the URL is readable/accesible, otherwise false.
is_readable(URL) ->
    {StreamMod, URIorURL} = get_mod_and_url(URL),
    case erlang:function_exported(StreamMod, is_readable, 1) of
        true -> StreamMod:is_readable(URIorURL);
        false -> false
    end.


-spec wildcard(pattern()) -> [uri()].
%% @doc returns the list of URIs for a given pattern or an empty list.
wildcard(Pattern) ->
    {StreamMod, URIorURL} = get_mod_and_url(Pattern),
    case erlang:function_exported(StreamMod, wildcard, 1) of
        true ->
            StreamMod:wildcard(URIorURL);
        false -> []
    end.

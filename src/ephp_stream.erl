-module(ephp_stream).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include_lib("kernel/include/file.hrl").
-include("ephp.hrl").

-export([
    parse_uri/1,
    get_res_id/1,
    open/2,
    close/1,
    read/2,
    write/3
]).

-type uri() :: binary().
-type option() :: atom() | {atom(), boolean() | integer() | binary()}.
-type options() :: [option()].

-type stream() :: binary(). %% file, http, ftp, ...

-export_type([uri/0, option/0, options/0, stream/0]).

-callback open(uri(), options()) -> {ok, pid()} | {error, reason()}.
-callback close(pid()) -> ok | {error, reason()}.
-callback read(pid(), options()) -> {ok, binary()} | eof | {error, reason()}.
-callback write(pid(), binary(), options()) -> ok | {error, reason()}.

-spec parse_uri(binary()) -> {stream(), uri()}.
%% @doc parse the URI to separate in stream and the rest of the URI.
parse_uri(URL) ->
    case binary:split(URL, <<":">>) of
        [URI] -> {<<"file">>, URI};
        [Schema, URI] -> {Schema, URI}
    end.


-spec get_res_id(resource()) -> integer().
%% @doc obtains the resource ID given a resource as param.
get_res_id(#resource{id = ID}) ->
    ID.


-spec open(uri(), options()) -> {ok, resource()} | {error, reason()}.
%% @doc open a stream given the schema, URI and options.
open(URL, Options) ->
    {Schema, URI} = parse_uri(URL),
    Prefix = ephp_config:get(<<"context.prefix_module">>, <<"ephp_stream_">>),
    {StreamMod, URIorURL} = try
        Module = binary_to_existing_atom(<<Prefix/binary, Schema/binary>>, utf8),
        case lists:member({open, 2}, Module:module_info(exports)) of
            true ->
                {Module, URI};
            false ->
                {binary_to_existing_atom(<<Prefix/binary, "file">>, utf8), URL}
        end
    catch
        error:badarg ->
            {binary_to_existing_atom(<<Prefix/binary, "file">>, utf8), URL}
    end,
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


-spec read(resource(), options()) -> {ok, binary()} | {error, reason()}.
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
    ID = case erlang:get(resource_next_id) of
        undefined -> 1;
        N -> N
    end,
    erlang:put(resource_next_id, ID + 1),
    ID.

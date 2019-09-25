-module(ephp_lib_file).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    handle_error/3,
    basename/3,
    dirname/3,
    file_exists/3,
    is_dir/3,
    is_readable/3,
    fopen/4,
    fclose/3,
    file_get_contents/3,
    fread/4,
    fwrite/5,
    fseek/5,
    feof/3,
    glob/4
]).

-include_lib("kernel/include/file.hrl").
-include("ephp.hrl").

-define(SEEK_SET, 0).
-define(SEEK_CUR, 1).
-define(SEEK_END, 2).

-define(GLOB_ERR, 4).
-define(GLOB_MARK, 8).
-define(GLOB_NOCHECK, 16).
-define(GLOB_NOSORT, 32).
-define(GLOB_BRACE, 128).
-define(GLOB_NOESCAPE, 8192).
-define(GLOB_ONLYDIR, 1073741824).

-define(DIRECTORY_SEPARATOR, <<"/">>).

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    basename,
    dirname,
    file_exists,
    {is_dir, [{args, [mixed]}]},
    {is_readable, [{args, [path]}]},
    {fopen, [
        %% TODO: bool $use_include_path=fase, resource $context
        {args, {2, 2, false, [string, string]}}
    ]},
    {fclose, [{args, [resource]}]},
    {file_get_contents, [
        {args, {1, 5, false, [string,
                              {boolean, false},
                              {resource, undefined},
                              {integer, 0},
                              {integer, undefined}]}},
        {pack_args, true}
    ]},
    {fread, [
        {args, {2, 2, false, [resource, integer]}}
    ]},
    {fwrite, [
        {args, {2, 3, false, [resource, string, {integer, 0}]}}
    ]},
    {fseek, [
        {args, {2, 3, -1, [resource, integer, {integer, ?SEEK_SET}]}}
    ]},
    {feof, [{args, {1, 1, false, [resource]}}]},
    {glob, [{args, {1, 2, false, [string, {integer, 0}]}}]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [
    {<<"SEEK_SET">>, ?SEEK_SET},
    {<<"SEEK_CUR">>, ?SEEK_CUR},
    {<<"SEEK_END">>, ?SEEK_END},
    {<<"GLOB_MARK">>, ?GLOB_MARK},
    {<<"GLOB_NOSORT">>, ?GLOB_NOSORT},
    {<<"GLOB_NOCHECK">>, ?GLOB_NOCHECK},
    {<<"GLOB_NOESCAPE">>, ?GLOB_NOESCAPE},
    {<<"GLOB_BRACE">>, ?GLOB_BRACE},
    {<<"GLOB_ONLYDIR">>, ?GLOB_ONLYDIR},
    {<<"GLOB_ERR">>, ?GLOB_ERR},
    {<<"DIRECTORY_SEPARATOR">>, ?DIRECTORY_SEPARATOR},
    {<<"PATH_SEPARATOR">>, ?DIRECTORY_SEPARATOR}
].

-spec handle_error(ephp_error:error_type(), ephp_error:error_level(),
                   Args::term()) -> string() | ignore.

handle_error(einval, _Level, {Function, DescriptorNum}) ->
    io_lib:format("~s(): ~p is not a valid stream resource",
                  [Function, DescriptorNum]);

handle_error(enoent, _Level, {Function, Filename}) ->
    io_lib:format("~s(~s): failed to open stream: No such file or directory",
                  [Function, Filename]);

handle_error(_Error, _Level, _Data) ->
    ignore.

-spec basename(context(), line(), var_value()) -> binary().

basename(_Context, _Line, {_Var, undefined}) ->
    <<>>;
basename(_Context, _Line, {_Var, PathFile}) ->
    filename:basename(ephp_data:to_bin(PathFile)).

-spec dirname(context(), line(), var_value()) -> binary().

dirname(_Context, _Line, {_Var, undefined}) ->
    <<>>;
dirname(_Context, _Line, {_Var, PathFile}) ->
    filename:dirname(ephp_data:to_bin(PathFile)).

-spec file_exists(context(), line(), var_value()) -> boolean().

file_exists(_Context, _Line, {_, Filename}) ->
    ephp_stream:file_exists(ephp_data:to_bin(Filename)).

-spec is_dir(context(), line(), var_value()) -> boolean().

is_dir(_Context, _Line, {_, Dirname}) ->
    ephp_stream:is_dir(ephp_data:to_bin(Dirname)).

-spec is_readable(context(), line(), var_value()) -> boolean().

is_readable(_Context, _Line, {_, Filename}) ->
    ephp_stream:is_readable(ephp_data:to_bin(Filename)).

-spec fopen(context(), line(), var_value(), var_value()) -> resource() | false.

fopen(Context, Line, {_, Filename}, {_, Mode}) ->
    %% TODO: trigger an error if the file has errors opening
    PHPOpts = lists:sort(binary_to_list(Mode)),
    Opts = case PHPOpts of
        "r" -> [read];
        "w" -> [write];
        "+r" -> [read, write];
        "+w" -> [read, write, truncate];
        "a" -> [append];
        "+a" -> [read, append];
        "x" -> [exclusive];
        "+x" -> [read, exclusive];
        %% FIXME: "c" uses flock to ensure exclusiveness while "x" is only open
        %%        the file if it doesn't exist. Erlang hasn't support for "c".
        "c" -> [append];
        "+c" -> [read, append];
        %% TODO: in PHP 7 was added "e": close-on-exec flag ??? POSIX.1-2008
        %"e" -> [];
        %% TODO: error???
        _ -> []
    end,
    case ephp_stream:open(Filename, Opts) of
        {ok, Resource} ->
            Resource;
        {error, enoent} ->
            Data = {<<"fopen">>, Filename},
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context, {error, enoent, Line, File,
                                              ?E_WARNING, Data}),
            false
    end.

-spec fclose(context(), line(), var_value()) -> boolean().

fclose(Context, Line, {_, Resource}) ->
    case ephp_stream:close(Resource) of
        ok ->
            true;
        {error, einval} ->
            DescriptorNum = ephp_stream:get_res_id(Resource),
            Data = {<<"fclose">>, DescriptorNum},
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context, {error, einval, Line, File,
                                              ?E_WARNING, Data}),
            false
    end.


-spec file_get_contents(context(), line(), [var_value()]) ->
      false | binary().

file_get_contents(_Context, _Line, [{_, File}, {_, false}, {_, undefined},
                                    {_, 0}, {_, undefined}]) ->
    case ephp_stream:read_file(File) of
        {ok, Data} ->
            Data;
        {error, _Reason} ->
            %% TODO error message???
            false
    end.


-spec fread(context(), line(), var_value(), var_value()) -> binary().

fread(_Context, _Line, {_, Resource}, {_, Length}) ->
    case ephp_stream:read(Resource, [{size, Length}]) of
        {error, _} ->
            %% TODO: maybe it requires to trigger an error
            <<>>;
        eof ->
            <<>>;
        {ok, Data} ->
            Data
    end.

-spec fwrite(context(), line(), var_value(), var_value(), var_value()) ->
      non_neg_integer() | false.

fwrite(Context, Line, Resource, {_, Data} = VData, {Length, 0}) ->
    fwrite(Context, Line, Resource, VData, {Length, byte_size(Data)});

fwrite(Context, Line, {_, Resource}, {_, Data}, {_, Length}) ->
    WriteData = case byte_size(Data) > Length of
        true -> <<Data:Length/binary>>;
        false -> Data
    end,
    case ephp_stream:write(Resource, WriteData, []) of
        ok ->
            byte_size(WriteData);
        {error, ebadf} ->
            0;
        {error, einval} ->
            DescriptorNum = ephp_stream:get_res_id(Resource),
            ErrData = {<<"fwrite">>, DescriptorNum},
            File = ephp_context:get_active_file(Context),
            ephp_error:handle_error(Context, {error, einval, Line, File,
                                              ?E_WARNING, ErrData}),
            false
    end.


-spec fseek(context(), line(), Handle::var_value(), Offset::var_value(),
            Whence::var_value()) -> 0 | -1.
%% @doc moves the file cursor to the specified offset.
fseek(_Context, _Line, {_, Resource}, {_, Offset}, {_, Whence}) ->
    Location = case Whence of
        ?SEEK_SET -> {bof, Offset};
        ?SEEK_CUR -> {cur, Offset};
        ?SEEK_END -> {eof, Offset};
        _ -> Offset
    end,
    case ephp_stream:position(Resource, Location) of
        ok -> 0;
        {error, _} -> -1
    end.


-spec feof(context(), line(), Resource::var_value()) -> boolean().
%% @doc returns true or false depending if EOF is achieved or not.
feof(_Context, _Line, {_, Resource}) ->
    ephp_stream:is_eof(Resource).


-spec glob(context(), line(), Pattern::var_value(), Flags::var_value()) ->
      ephp_array().

glob(_Context, _Line, {_, Pattern}, {_, Flags}) ->
    Files = ephp_stream:wildcard(ephp_data:to_bin(Pattern)),
    PrFiles = glob_flags(Pattern, Files, Flags),
    ephp_array:from_list(PrFiles).


glob_flags(Pattern, Files, Flags) when Flags band ?GLOB_MARK > 0 ->
    NewFiles = lists:map(fun(File) ->
        case ephp_stream:is_dir(File) of
            true -> <<File/binary, "/">>;
            false -> File
        end
    end, Files),
    glob_flags(Pattern, NewFiles, Flags band (bnot ?GLOB_MARK));

glob_flags(Pattern, Files, Flags) when Flags band ?GLOB_ONLYDIR > 0 ->
    Dirs = [ File || File <- Files, filelib:is_dir(File) ],
    glob_flags(Pattern, Dirs, Flags band (bnot ?GLOB_ONLYDIR));

glob_flags(Pattern, [], Flags) when Flags band ?GLOB_NOCHECK > 0 ->
    glob_flags(Pattern, [Pattern], Flags band (bnot ?GLOB_NOCHECK));

glob_flags(Pattern, Files, Flags) when Flags band ?GLOB_NOCHECK > 0 ->
    glob_flags(Pattern, Files, Flags band (bnot ?GLOB_NOCHECK));

glob_flags(_Pattern, Files, ?GLOB_NOSORT) ->
    Files;

glob_flags(_Pattern, Files, 0) ->
    lists:sort(Files).

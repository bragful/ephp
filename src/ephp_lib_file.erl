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
    fread/4,
    fwrite/5
]).

-include_lib("kernel/include/file.hrl").
-include("ephp.hrl").

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
    {fread, [
        {args, {2, 2, false, [resource, integer]}}
    ]},
    {fwrite, [
        {args, {2, 3, false, [resource, string, {integer, 0}]}}
    ]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

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
    filelib:is_regular(Filename).

-spec is_dir(context(), line(), var_value()) -> boolean().

is_dir(_Context, _Line, {_, Dirname}) ->
    filelib:is_dir(ephp_data:to_bin(Dirname)).

-spec is_readable(context(), line(), var_value()) -> boolean().

is_readable(_Context, _Line, {_, Filename}) ->
    case file:read_file_info(Filename) of
        {ok, #file_info{access = Access}} when Access =:= read
                                          orelse Access =:= read_write ->
            true;
        _ ->
            false
    end.

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

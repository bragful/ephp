-module(ephp_lib_exec).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    handle_error/3,
    shell_exec/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {shell_exec, [{args, [string]}]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec handle_error(ephp_error:error_type(), ephp_error:error_level(),
                   Args::term()) -> string() | ignore.

handle_error(_Error, _Level, _Data) ->
    ignore.

-spec shell_exec(context(), line(), var_value()) -> binary().

shell_exec(_Context, _Line, {_, BaseCommand}) ->
    %% TODO: give more security to this and ensure it's not disabled
    EscapedCommand = ephp_string:escape(ephp_data:to_bin(BaseCommand), $"),
    Command = binary_to_list(<<"sh -c ", EscapedCommand/binary>>),
    iolist_to_binary(os:cmd(Command)).

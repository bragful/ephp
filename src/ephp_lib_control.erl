-module(ephp_lib_control).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    handle_error/3,
    include/3,
    include_once/3,
    require/3,
    require_once/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {include, [{arg, [string]}]},
    {include_once, [{arg, [string]}]},
    {require, [{arg, [string]}]},
    {require_once, [{arg, [string]}]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [
    {<<"include_path">>, <<".:">>}
].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec handle_error(ephp_error:error_type(), ephp_error:error_level(),
                   Args::term()) -> string() | ignore.

handle_error(erequired, _Level, {ReqFile, Func}) ->
    IncludePath = ephp_config:get(<<"include_path">>, <<".:">>),
    io_lib:format(
        "~s(): Failed opening required '~s' (include_path='~s')",
        [Func, ReqFile, IncludePath]);

handle_error(einclude, _Level, {ReqFile, Func}) ->
    IncludePath = ephp_config:get(<<"include_path">>, <<".:">>),
    io_lib:format(
        "~s(): Failed opening '~s' for inclusion (include_path='~s')",
        [Func, ReqFile, IncludePath]);

handle_error(_Type, _Level, _Args) ->
    ignore.

-spec include(context(), line(), InclFile :: var_value()) -> any().

include(Context, Line, {Var, InclFile}) ->
    File = ephp_context:get_active_file(Context),
    case ephp_context:load(Context, InclFile) of
        {error, _} ->
            NoFileData = {InclFile, <<"include">>},
            ephp_error:handle_error(Context, {error, enofile, Line, File,
                ?E_WARNING, NoFileData}),
            ephp_error:handle_error(Context, {error, einclude, Line, File,
                ?E_WARNING, NoFileData}),
            undefined;
        Code ->
            Args = case Var of
                #variable{} ->
                    [#var_ref{pid = ephp_context:get_vars(Context), ref = Var}];
                #var_ref{} ->
                    [Var];
                _ ->
                    [InclFile]
            end,
            ephp_stack:push(Context, File, Line, <<"include">>, Args,
                            undefined, undefined),
            Return = include_file(Context, Code, InclFile),
            ephp_stack:pop(Context),
            Return
    end.

-spec include_once(context(), line(), File :: var_value()) -> any().

include_once(Context, Line, {Var, InclFile}) ->
    File = ephp_context:get_active_file(Context),
    case ephp_context:load_once(Context, InclFile) of
        {error, _} ->
            NoFileData = {InclFile, <<"include_once">>},
            ephp_error:handle_error(Context, {error, enofile, Line, File,
                ?E_WARNING, NoFileData}),
            ephp_error:handle_error(Context, {error, einclude, Line, File,
                ?E_WARNING, NoFileData}),
            undefined;
        {return, true} ->
            true;
        Code ->
            Args = case Var of
                #variable{} ->
                    [#var_ref{pid = ephp_context:get_vars(Context), ref = Var}];
                #var_ref{} ->
                    [Var];
                _ ->
                    [InclFile]
            end,
            ephp_stack:push(Context, File, Line, <<"include_once">>, Args,
                            undefined, undefined),
            Return = include_file(Context, Code, InclFile),
            ephp_stack:pop(Context),
            Return
    end.

-spec require(context(), line(), File :: var_value()) -> any().

require(Context, Line, {Var, ReqFile}) ->
    File = ephp_context:get_active_file(Context),
    case ephp_context:load(Context, ReqFile) of
        {error, _} ->
            NoFileData = {ReqFile, <<"require">>},
            ephp_error:handle_error(Context, {error, enofile, Line, File,
                ?E_WARNING, NoFileData}),
            ephp_error:error({error, erequired, Line, ?E_ERROR, NoFileData});
        Code ->
            Args = case Var of
                #variable{} ->
                    [#var_ref{pid = ephp_context:get_vars(Context), ref = Var}];
                #var_ref{} ->
                    [Var];
                _ ->
                    [ReqFile]
            end,
            ephp_stack:push(Context, File, Line, <<"require">>, Args,
                            undefined, undefined),
            Return = include_file(Context, Code, ReqFile),
            ephp_stack:pop(Context),
            Return
    end.

-spec require_once(context(), line(), File :: var_value()) -> any().

require_once(Context, Line, {Var, ReqFile}) ->
    File = ephp_context:get_active_file(Context),
    case ephp_context:load_once(Context, ReqFile) of
        {error, _} ->
            NoFileData = {ReqFile, <<"require_once">>},
            ephp_error:handle_error(Context, {error, enofile, Line, File,
                ?E_WARNING, NoFileData}),
            ephp_error:error({error, erequired, Line, ?E_ERROR, NoFileData});
        {return, true} ->
            true;
        Code ->
            Args = case Var of
                #variable{} ->
                    [#var_ref{pid = ephp_context:get_vars(Context), ref = Var}];
                #var_ref{} ->
                    [Var];
                _ ->
                    [ReqFile]
            end,
            ephp_stack:push(Context, File, Line, <<"require_once">>, Args,
                            undefined, undefined),
            Return = include_file(Context, Code, ReqFile),
            ephp_stack:pop(Context),
            Return
    end.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

include_file(Context, Code, File) ->
    OldValue = ephp_context:get_active_file(Context),
    ephp_context:set_active_file(Context, File),
    {ok, Res} = ephp_interpr:process(Context, Code),
    ephp_context:set_active_file(Context, OldValue),
    case Res of
        {return, Value} -> Value;
        _ -> undefined
    end.

-module(ephp_lib_control).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    include/3,
    include_once/3,
    require/3,
    require_once/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    include, include_once,
    require, require_once
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [
    {<<"include_path">>, <<".:">>}
].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec include(context(), line(), InclFile :: var_value()) -> any().

include(Context, Line, {_,InclFile}) ->
    case ephp_context:load(Context, InclFile) of
    {error, _} ->
        File = ephp_context:get_active_file(Context),
        NoFileData = {File, InclFile, <<"include">>},
        ephp_error:handle_error(Context, {error, enofile, Line, ?E_WARNING,
            NoFileData}),
        ephp_error:handle_error(Context, {error, einclude, Line, ?E_WARNING,
            NoFileData}),
        undefined;
    Code ->
        include_file(Context, Code, InclFile)
    end.

-spec include_once(context(), line(), File :: var_value()) -> any().

include_once(Context, Line, {_,InclFile}) ->
    case ephp_context:load_once(Context, InclFile) of
    {error, _} ->
        File = ephp_context:get_active_file(Context),
        NoFileData = {File, InclFile, <<"include_once">>},
        ephp_error:handle_error(Context, {error, enofile, Line, ?E_WARNING,
            NoFileData}),
        ephp_error:handle_error(Context, {error, einclude, Line, ?E_WARNING,
            NoFileData}),
        undefined;
    {return, true} ->
        true;
    Code ->
        include_file(Context, Code, InclFile)
    end.

-spec require(context(), line(), File :: var_value()) -> any().

require(Context, Line, {_,ReqFile}) ->
    case ephp_context:load(Context, ReqFile) of
    {error, _} ->
        File = ephp_context:get_active_file(Context),
        NoFileData = {File, ReqFile, <<"require">>},
        ephp_error:handle_error(Context, {error, enofile, Line, ?E_WARNING,
            NoFileData}),
        ephp_error:error({error, erequired, Line, ?E_ERROR, NoFileData});
    Code ->
        include_file(Context, Code, ReqFile)
    end.

-spec require_once(context(), line(), File :: var_value()) -> any().

require_once(Context, Line, {_,ReqFile}) ->
    case ephp_context:load_once(Context, ReqFile) of
    {error, _} ->
        File = ephp_context:get_active_file(Context),
        NoFileData = {File, ReqFile, <<"require_once">>},
        ephp_error:handle_error(Context, {error, enofile, Line, ?E_WARNING,
            NoFileData}),
        ephp_error:error({error, erequired, Line, ?E_ERROR, NoFileData});
    {return, true} ->
        true;
    Code ->
        include_file(Context, Code, ReqFile)
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

-module(ephp_lib_control).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    include/3,
    include_once/3,
    require/3,
    require_once/3
]).

-include("ephp.hrl").

-spec init() -> [ephp_func:php_function()].

init() -> [
    include, include_once,
    require, require_once
]. 

-spec include(context(), line(), InclFile :: var_value()) -> any().

include(Context, Line, {_,InclFile}) ->
    case ephp_context:load(Context, InclFile) of
    {error, _} ->
        File = ephp_context:get_active_file(Context),
        NoFileData = {File, InclFile, <<"include">>},
        IncludeData = {File, InclFile, <<"include">>},
        ephp_error:handle_error(Context, {error, enofile, Line, ?E_WARNING,
            NoFileData}),
        ephp_error:handle_error(Context, {error, einclude, Line, ?E_WARNING,
            IncludeData}),
        null;
    Code -> 
        include_file(Context, Code, InclFile)
    end.

-spec include_once(context(), line(), File :: var_value()) -> any().

include_once(Context, Line, {_,InclFile}) ->
    case ephp_context:load_once(Context, InclFile) of
    {error, _} ->
        File = ephp_context:get_active_file(Context),
        NoFileData = {File, InclFile, <<"include_once">>},
        IncludeData = {File, InclFile, <<"include_once">>},
        ephp_error:handle_error(Context, {error, enofile, Line, ?E_WARNING,
            NoFileData}),
        ephp_error:handle_error(Context, {error, einclude, Line, ?E_WARNING,
            IncludeData}),
        null;
    {return, true} ->
        true;
    Code -> 
        include_file(Context, Code, InclFile)
    end.

-spec require(context(), line(), File :: var_value()) -> any().

require(Context, Line, {_,File}) ->
    case ephp_context:load(Context, File) of
    {error, _} ->
        ephp_error:error({error, erequired, Line, ?E_ERROR, File});
    Code -> 
        include_file(Context, Code, File)
    end.

-spec require_once(context(), line(), File :: var_value()) -> any().

require_once(Context, Line, {_,File}) ->
    case ephp_context:load_once(Context, File) of
    {error, _} ->
        ephp_error:error({error, erequired, Line, ?E_ERROR, File});
    {return, true} ->
        true;
    Code ->
        include_file(Context, Code, File)
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
        _ -> null
    end.

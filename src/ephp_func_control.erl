-module(ephp_func_control).
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
        File = ephp_context:get_const(Context, <<"__FILE__">>),
        NoFileData = {File, InclFile, <<"include">>},
        IncludeData = {File, InclFile, <<"include">>},
        ephp_error:handle_error(Context, {error, enofile, Line, NoFileData}),
        ephp_error:handle_error(Context, {error, einclude, Line, IncludeData}),
        null;
    Code -> 
        include_file(Context, Code, InclFile)
    end.

-spec include_once(context(), line(), File :: var_value()) -> any().

include_once(Context, Line, {_,InclFile}) ->
    case ephp_context:load_once(Context, InclFile) of
    {error, _} ->
        File = ephp_context:get_const(Context, <<"__FILE__">>),
        NoFileData = {File, InclFile, <<"include_once">>},
        IncludeData = {File, InclFile, <<"include_once">>},
        ephp_error:handle_error(Context, {error, enofile, Line, NoFileData}),
        ephp_error:handle_error(Context, {error, einclude, Line, IncludeData}),
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
        throw({error, erequired, Line, File});
    Code -> 
        include_file(Context, Code, File)
    end.

-spec require_once(context(), line(), File :: var_value()) -> any().

require_once(Context, Line, {_,File}) ->
    case ephp_context:load_once(Context, File) of
    {error, _} ->
        throw({error, erequired, Line, File});
    {return, true} ->
        true;
    Code ->
        include_file(Context, Code, File)
    end.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

include_file(Context, Code, File) ->
    OldValue = ephp_context:get_const(Context, <<"__FILE__">>),
    ephp_context:register_const(Context, <<"__FILE__">>, File),
    {ok, Res} = ephp_interpr:process(Context, Code), 
    ephp_context:register_const(Context, <<"__FILE__">>, OldValue),
    case Res of
        {return, Value} -> Value;
        _ -> null
    end.

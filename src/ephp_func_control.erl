-module(ephp_func_control).
-compile([warnings_as_errors]).

-export([
    init/1,
    include/2,
    include_once/2,
    require/2,
    require_once/2
]).

-include("ephp.hrl").

-spec init(Context :: context()) -> ok.

init(Context) ->
    Funcs = [
        include, include_once,
        require, require_once
    ],
    lists:foreach(fun(Func) ->
        Name = atom_to_binary(Func, utf8),
        ephp:register_func(Context, Name, ?MODULE, Func)  
    end, Funcs), 
    ok. 

-spec include(Context :: context(), File :: var_value()) -> any().

include(Context, {_,File}) ->
    case ephp_context:load(Context, File) of
    {error, _} -> null;
    Code -> 
        OldValue = ephp_context:get_const(Context, <<"__FILE__">>),
        ephp_context:register_const(Context, <<"__FILE__">>, File),
        {ok, Res} = ephp_interpr:process(Context, Code), 
        ephp_context:register_const(Context, <<"__FILE__">>, OldValue),
        case Res of
            {return, Value} -> Value;
            _ -> null
        end
    end.

-spec include_once(Context :: context(), File :: var_value()) -> any().

include_once(Context, {_,File}) ->
    case ephp_context:load_once(Context, File) of
    {error, _} -> null;
    {return, true} ->
        true;
    Code -> 
        OldValue = ephp_context:get_const(Context, <<"__FILE__">>),
        ephp_context:register_const(Context, <<"__FILE__">>, File),
        {ok, Res} = ephp_interpr:process(Context, Code), 
        ephp_context:register_const(Context, <<"__FILE__">>, OldValue),
        case Res of
            {return, Value} -> Value;
            _ -> null
        end
    end.

-spec require(Context :: context(), File :: var_value()) -> any().

require(Context, {_,File}) ->
    case ephp_context:load(Context, File) of
    {error, _} -> throw({erequired, File});
    Code -> 
        OldValue = ephp_context:get_const(Context, <<"__FILE__">>),
        ephp_context:register_const(Context, <<"__FILE__">>, File),
        {ok, Res} = ephp_interpr:process(Context, Code), 
        ephp_context:register_const(Context, <<"__FILE__">>, OldValue),
        case Res of
            {return, Value} -> Value;
            _ -> null
        end
    end.

-spec require_once(Context :: context(), File :: var_value()) -> any().

require_once(Context, {_,File}) ->
    case ephp_context:load_once(Context, File) of
    {error, _} -> throw(erequired);
    {return, true} ->
        true;
    Code -> 
        OldValue = ephp_context:get_const(Context, <<"__FILE__">>),
        ephp_context:register_const(Context, <<"__FILE__">>, File),
        {ok, Res} = ephp_interpr:process(Context, Code), 
        ephp_context:register_const(Context, <<"__FILE__">>, OldValue),
        case Res of
            {return, Value} -> Value;
            _ -> null
        end
    end.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------


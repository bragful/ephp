-module(ephp_config).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    start_link/1,

    get/1,
    get/2,

    set/2,

    read_config/1,
    module_init/1
]).

-spec start_link(file_name()) -> ok.

start_link(File) ->
    case read_config(File) of
        {ok, Config} ->
            lists:foreach(fun
                ({<<"extension">>, Module}) ->
                    % TODO notice when the extension is duplicated
                    add_module(Module);
                ({K, V}) ->
                    application:set_env(ephp, K, V)
            end, Config);
        _ ->
            ok
    end,
    ok.

-spec get(Key :: binary()) -> mixed().

get(Key) ->
    application:get_env(ephp, Key, undefined).

-spec get(Key :: binary(), Default :: mixed()) -> mixed().

get(Key, Default) ->
    application:get_env(ephp, Key, Default).

-spec set(Key :: binary(), Value :: mixed()) -> ok.

set(Key, Value) ->
    application:set_env(ephp, Key, Value),
    ok.

-spec read_config(file_name()) -> proplists:proplists().

read_config(File) ->
    case zucchini:parse_file(File) of
        {ok, Content} ->
            {ok, lists:foldl(fun({_S,C},R) ->
                R ++ C
            end, [], Content)};
        {error,enoent} ->
            {error, enoent}
    end.

-spec module_init(module()) -> ok.

module_init(Module) ->
    lists:foreach(fun({K,V}) ->
        case application:get_env(ephp, K, undefined) of
            undefined ->
                application:set_env(ephp, K, V);
            _ ->
                ok
        end
    end, Module:init_config()).


%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

-spec add_module(binary()) -> ok.

add_module(Extension) ->
    Module = case binary:split(Extension, <<".">>) of
        [ExtName, <<"so">>] ->
            % TODO: ensure the ExtName hasn't path
            binary_to_atom(<<"ephp_lib_",ExtName/binary>>, utf8);
        [<<"php_", ExtName/binary>>, <<"dll">>] ->
            binary_to_atom(<<"ephp_lib_",ExtName/binary>>, utf8);
        % TODO: ExtName without path and Windows format
        [ExtName, <<"dll">>] ->
            % TODO: ensure the ExtName hasn't path
            binary_to_atom(<<"ephp_lib_",ExtName/binary>>, utf8)
    end,
    Modules = application:get_env(ephp, modules, []),
    case lists:member(Module, Modules) of
        true ->
            {error, duplicated};
        false ->
            application:set_env(ephp, modules, [Module|Modules]),
            ok
    end.

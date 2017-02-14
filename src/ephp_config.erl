-module(ephp_config).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").

-export([
    start_link/0,
    start_link/1,

    get/1,
    get/2,

    set/2,

    read_config/1,
    get_defaults/0
]).

-spec start_link() -> ok.

start_link() ->
    set_defaults().

-spec start_link(file_name()) -> ok.

start_link(File) ->
    % ensure the application ephp is started
    ephp:start(),
    set_defaults(),
    case read_config(File) of
        {ok, Config} ->
            lists:foreach(fun
                ({<<"extension">>, Module}) -> add_module(Module);
                ({K, V}) -> application:set_env(ephp, K, V)
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

-spec get_defaults() -> proplists:proplists().

get_defaults() ->
    lists:flatmap(fun(Module) ->
        Module:init_config()
    end, application:get_env(ephp, modules, [])).

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
            binary_to_atom(<<"ephp_lib_",ExtName/binary>>, utf8)
        % TODO: ExtName without path and Windows format
    end,
    init_config(Module),
    % TODO notice when the extension is duplicated
    Modules = application:get_env(ephp, modules, ?MODULES),
    application:set_env(ephp, modules, [Module|Modules]),
    ok.

-spec set_defaults() -> ok.

set_defaults() ->
    application:set_env(ephp, modules, ?MODULES),
    lists:foreach(fun init_config/1, ?MODULES).

-spec init_config(module()) -> ok.

init_config(Module) ->
    [ application:set_env(ephp, K, V) || {K,V} <- Module:init_config() ],
    ok.

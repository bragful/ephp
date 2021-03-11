-module(ephp_config).

-author('manuel@altenwald.com').

-include("ephp.hrl").

-export([start_link/1, start_local/0, stop_local/0, get_all/0, get/1, get/2, get_bool/1,
         get_bool/2, get_atom/1, set/2, read_config/1, module_init/1]).

-spec start_link(file_name()) -> ok.
start_link(File) ->
    case read_config(File) of
        {ok, Config} ->
            lists:foreach(fun ({<<"extension">>, Module}) ->
                                  % TODO notice when the extension is duplicated
                                  add_module(Module);
                              ({K, V}) ->
                                  application:set_env(ephp, K, V)
                          end,
                          Config);
        _ ->
            ok
    end,
    ok.

-spec start_local() -> ok.
start_local() ->
    Config =
        lists:foldl(fun({K, V}, Dict) -> dict:store(K, V, Dict) end,
                    dict:new(),
                    application:get_all_env(ephp)),
    erlang:put(ephp_config, Config),
    ok.

-spec stop_local() -> ok.
stop_local() ->
    erlang:erase(ephp_config),
    ok.

-type config_key() :: binary() | atom().

-spec get_all() -> [{config_key(), mixed()}].
get_all() ->
    AllConfig =
        case erlang:get(ephp_config) of
            undefined ->
                application:get_all_env(ephp);
            AllConfigDict ->
                dict:to_list(AllConfigDict)
        end,
    [Cfg || {K, _} = Cfg <- AllConfig, is_binary(K) or is_integer(K)].

-spec get(config_key()) -> mixed().
get(Key) ->
    get(Key, undefined).

-spec get(binary(), undefined | mixed()) -> undefined | mixed();
         (atom(), undefined | binary()) -> undefined | binary();
         (atom(), [atom()]) -> [atom()].
get(Key, Default) ->
    case erlang:get(ephp_config) of
        undefined ->
            application:get_env(ephp, Key, Default);
        Config ->
            case dict:find(Key, Config) of
                error ->
                    application:get_env(ephp, Key, Default);
                {ok, Value} ->
                    Value
            end
    end.

-spec get_bool(config_key()) -> boolean().
get_bool(Key) ->
    get_bool(Key, false).

-spec get_bool(config_key(), Default :: boolean()) -> boolean().
get_bool(Key, Default) ->
    case get(Key, Default) of
        Val when is_boolean(Val) ->
            Val;
        Text ->
            case ephp_string:to_lower(Text) of
                <<"on">> ->
                    true;
                <<"off">> ->
                    false;
                <<"yes">> ->
                    true;
                <<"no">> ->
                    false;
                <<"1">> ->
                    true;
                <<"0">> ->
                    false;
                Other ->
                    ephp_data:to_bool(Other)
            end
    end.

-spec get_atom(config_key()) -> atom().
get_atom(Key) ->
    case get(Key, undefined) of
        Val when is_binary(Val) ->
            binary_to_atom(Val, utf8);
        Val when is_atom(Val) ->
            Val
    end.

-spec set(config_key(), Value :: mixed() | [atom()]) -> ok.
set(Key, Value) ->
    case erlang:get(ephp_config) of
        undefined ->
            application:set_env(ephp, Key, Value);
        Config ->
            erlang:put(ephp_config, dict:store(Key, Value, Config))
    end,
    ok.

-spec read_config(file_name()) -> proplists:proplists().
read_config(File) ->
    case zucchini:parse_file(File) of
        {ok, Content} ->
            {ok, lists:foldl(fun({_S, C}, R) -> R ++ C end, [], Content)};
        {error, enoent} ->
            {error, enoent}
    end.

-spec module_init(module()) -> ok.
module_init(Module) ->
    lists:foreach(fun({K, V}) ->
                     case application:get_env(ephp, K, undefined) of
                         undefined ->
                             application:set_env(ephp, K, V);
                         _ ->
                             ok
                     end
                  end,
                  Module:init_config()).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

-spec add_module(binary()) -> ok.
add_module(Extension) ->
    Module =
        case binary:split(Extension, <<".">>) of
            [ExtName, <<"so">>] ->
                % TODO: ensure the ExtName hasn't path
                binary_to_atom(<<"ephp_lib_", ExtName/binary>>, utf8);
            [<<"php_", ExtName/binary>>, <<"dll">>] ->
                binary_to_atom(<<"ephp_lib_", ExtName/binary>>, utf8);
            % TODO: ExtName without path and Windows format
            [ExtName, <<"dll">>] ->
                % TODO: ensure the ExtName hasn't path
                binary_to_atom(<<"ephp_lib_", ExtName/binary>>, utf8)
        end,
    Modules = application:get_env(ephp, modules, []),
    case lists:member(Module, Modules) of
        true ->
            {error, duplicated};
        false ->
            application:set_env(ephp, modules, [Module | Modules]),
            ok
    end.

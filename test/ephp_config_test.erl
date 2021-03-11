-module(ephp_config_test).

-author('manuel@altenwald.com').

-compile([warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").

-include("ephp.hrl").

config_wrong_file_test() ->
    ok = ephp_config:start_link("test/php_test.ini_WRONG"),
    ok = ephp_config:start_local(),
    ?assertEqual(14, ephp_config:get(<<"precision">>)),
    ?assertNot(lists:member(ephp_lib_test, ephp_config:get(modules))),
    ?assertEqual(undefined, ephp_config:get(<<"test">>)),
    ok = ephp_config:stop_local(),
    ok.

config_test() ->
    ok = ephp_config:start_link("test/php_test.ini"),
    ok = ephp_config:start_local(),
    ok = ephp_config:module_init(ephp_lib_test),
    ?assertEqual(20, ephp_config:get(<<"precision">>)),
    ?assert(lists:member(ephp_lib_test, ephp_config:get(modules))),
    ?assertEqual(<<"true">>, ephp_config:get(<<"test">>)),
    ok = ephp_config:stop_local(),
    ok.

config_change_local_test() ->
    ok = ephp_config:start_link("test/php_test.ini"),

    % start first time
    ok = ephp_config:start_local(),
    ?assertEqual(20, ephp_config:get(<<"precision">>)),
    ok = ephp_config:set(<<"precision">>, 14),
    ?assertEqual(14, ephp_config:get(<<"precision">>)),

    % start second time
    ok = ephp_config:start_local(),
    ?assertEqual(20, ephp_config:get(<<"precision">>)),
    ok = ephp_config:stop_local(),
    ok.

config_only_global_test() ->
    ok = ephp_config:start_link("test/php_test.ini"),
    ?assertEqual(20, ephp_config:get(<<"precision">>)),
    ok = ephp_config:set(<<"precision">>, 14),
    ?assertEqual(14, ephp_config:get(<<"precision">>)),
    ok.

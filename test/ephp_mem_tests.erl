-module(ephp_mem_tests).

-include_lib("eunit/include/eunit.hrl").

set_get_and_remove_test() ->
    {ok, _} = ephp_mem:start_link(),
    ?assertEqual(ok, ephp_mem:set(1, 100)),
    ?assertEqual(100, ephp_mem:get(1)),
    ?assertEqual(ok, ephp_mem:remove(1)),
    ?assertException(throw, segmentation_fault, ephp_mem:get(1)),
    ?assertEqual(ok, ephp_mem:set(1, 100)),
    ?assertEqual(100, ephp_mem:get(1)),
    ?assertEqual(ok, ephp_mem:stop()),
    ?assertException(error, badarg, ephp_mem:get(1)),
    ok.

add_test() ->
    {ok, _} = ephp_mem:start_link(),
    ?assertEqual(1, ephp_mem:add(100)),
    ?assertEqual(2, ephp_mem:add(200)),
    ?assertEqual(3, ephp_mem:add(300)),
    ?assertEqual(4, ephp_mem:add(400)),
    ?assertEqual(ok, ephp_mem:remove(2)),
    ?assertEqual(ok, ephp_mem:remove(4)),
    ?assertEqual(2, ephp_mem:add(20)),
    ?assertEqual(4, ephp_mem:add(40)),
    ok = ephp_mem:stop(),
    ?assertException(error, badarg, ephp_mem:get(1)),
    ok.

add_remove_links_test() ->
    {ok, _} = ephp_mem:start_link(),
    ?assertEqual(1, ephp_mem:add(100)),
    ?assertEqual(ok, ephp_mem:add_link(1)),
    ?assertEqual(ok, ephp_mem:add_link(1)),
    ?assertEqual(ok, ephp_mem:remove(1)),
    ?assertEqual(ok, ephp_mem:remove(1)),
    ?assertEqual(100, ephp_mem:get(1)),
    ?assertEqual(ok, ephp_mem:remove(1)),
    ?assertException(throw, segmentation_fault, ephp_mem:remove(1)),
    ok = ephp_mem:stop(),
    ?assertException(error, badarg, ephp_mem:get(1)),
    ok.

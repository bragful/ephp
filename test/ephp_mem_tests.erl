-module(ephp_mem_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ephp.hrl").

set_get_and_remove_test() ->
    {ok, _} = ephp_mem:start_link(),
    MemRef = #mem_ref{mem_id = 1},
    ?assertEqual(ok, ephp_mem:set(MemRef, 100)),
    ?assertEqual(100, ephp_mem:get(MemRef)),
    ?assertEqual(ok, ephp_mem:remove(MemRef)),
    ?assertException(throw, segmentation_fault, ephp_mem:get(MemRef)),
    ?assertEqual(ok, ephp_mem:set(MemRef, 100)),
    ?assertEqual(100, ephp_mem:get(MemRef)),
    ?assertEqual(ok, ephp_mem:stop()),
    ?assertException(error, badarg, ephp_mem:get(MemRef)),
    ok.

add_test() ->
    {ok, _} = ephp_mem:start_link(),
    ?assertEqual(#mem_ref{mem_id = 1}, ephp_mem:add(100)),
    ?assertEqual(#mem_ref{mem_id = 2}, ephp_mem:add(200)),
    ?assertEqual(#mem_ref{mem_id = 3}, ephp_mem:add(300)),
    ?assertEqual(#mem_ref{mem_id = 4}, ephp_mem:add(400)),
    ?assertEqual(ok, ephp_mem:remove(#mem_ref{mem_id = 2})),
    ?assertEqual(ok, ephp_mem:remove(#mem_ref{mem_id = 4})),
    ?assertEqual(#mem_ref{mem_id = 2}, ephp_mem:add(20)),
    ?assertEqual(#mem_ref{mem_id = 4}, ephp_mem:add(40)),
    ok = ephp_mem:stop(),
    ?assertException(error, badarg, ephp_mem:get(#mem_ref{mem_id = 1})),
    ok.

add_remove_links_test() ->
    {ok, _} = ephp_mem:start_link(),
    MemRef = #mem_ref{mem_id = 1},
    ?assertEqual(MemRef, ephp_mem:add(100)),
    ?assertEqual(ok, ephp_mem:add_link(MemRef)),
    ?assertEqual(ok, ephp_mem:add_link(MemRef)),
    ?assertEqual(ok, ephp_mem:remove(MemRef)),
    ?assertEqual(100, ephp_mem:get(MemRef)),
    ?assertEqual(ok, ephp_mem:remove(MemRef)),
    ?assertException(throw, segmentation_fault, ephp_mem:remove(MemRef)),
    ok = ephp_mem:stop(),
    ?assertException(error, badarg, ephp_mem:get(MemRef)),
    ok.

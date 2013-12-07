-module(ephp_context_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

create_and_stop_test() ->
    {ok, Ctx} = ephp_context:start_link(),
    ?assert(is_process_alive(Ctx)),
    ephp_context:destroy(Ctx),
    timer:sleep(500), 
    ?assertNot(is_process_alive(Ctx)).

setup_test_() ->
    {foreach,
        fun start/0,
        fun stop/1, [
        fun set_and_get_test/1,
        fun set_and_get_array_test/1
    ]}.

start() ->
    {ok, Ctx} = ephp_context:start_link(),
    Ctx.

stop(Ctx) ->
    ephp_context:destroy(Ctx). 

set_and_get_test(Ctx) -> [
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"a">>,[]},10)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"b">>,[]},20)),
    ?_assertEqual(10, ephp_context:get(Ctx,{variable,<<"a">>,[]})),
    ?_assertEqual(20, ephp_context:get(Ctx,{variable,<<"b">>,[]}))
].

set_and_get_array_test(Ctx) -> [
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"a">>,[{int,0}]},10)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"a">>,[{int,1}]},20)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"a">>,[{text,<<"hola">>}]},30)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"a">>,[{int,2},{int,0}]},40)),
    ?_assertEqual(10, ephp_context:get(Ctx,{variable,<<"a">>,[{int,0}]})),
    ?_assertEqual(20, ephp_context:get(Ctx,{variable,<<"a">>,[{int,1}]})),
    ?_assertEqual(30, ephp_context:get(Ctx,{variable,<<"a">>,[{text,<<"hola">>}]})),
    ?_assertEqual(40, ephp_context:get(Ctx,{variable,<<"a">>,[{int,2},{int,0}]}))
].

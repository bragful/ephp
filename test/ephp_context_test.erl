-module(ephp_context_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("ephp.hrl").

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
        fun set_and_get_array_test/1,
        fun resolve_assign/1,
        fun operation_test/1,
        fun arith_mono_test/1,
        fun global_test/1
    ]}.

start() ->
    {ok, Ctx} = ephp_context:start_link(),
    Ctx.

stop(Ctx) ->
    ephp_context:destroy(Ctx). 

set_and_get_test(Ctx) -> [
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"a">>,[]},10)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"b">>,[]},20)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"c">>,[]},undefined)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"d">>,[]},true)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"e">>,[]},false)),

    ?_assertEqual(10, ephp_context:get(Ctx,{variable,<<"a">>,[]})),
    ?_assertEqual(20, ephp_context:get(Ctx,{variable,<<"b">>,[]})),
    ?_assertEqual(undefined, ephp_context:get(Ctx,{variable,<<"c">>,[]})),
    ?_assertEqual(true, ephp_context:get(Ctx,{variable,<<"d">>,[]})),
    ?_assertEqual(false, ephp_context:get(Ctx,{variable,<<"e">>,[]}))
].

set_and_get_array_test(Ctx) -> [
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"a">>,[{int,0}]},10)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"a">>,[{int,1}]},20)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"a">>,[{text,<<"hello">>}]},30)),
    ?_assertEqual(ok, ephp_context:set(Ctx,{variable,<<"a">>,[{int,2},{int,0}]},40)),
    ?_assertEqual(10, ephp_context:get(Ctx,{variable,<<"a">>,[0]})),
    ?_assertEqual(20, ephp_context:get(Ctx,{variable,<<"a">>,[1]})),
    ?_assertEqual(30, ephp_context:get(Ctx,{variable,<<"a">>,[<<"hello">>]})),
    ?_assertEqual(40, ephp_context:get(Ctx,{variable,<<"a">>,[2,0]}))
].

resolve_assign(Ctx) -> [
    ?_assertEqual(10, ephp_context:solve(Ctx,#assign{variable=#variable{name = <<"a">>}, expression=#int{int=10}}))
].

operation_test(Ctx) -> [
    ?_assertEqual(10, ephp_context:solve(Ctx, #operation{type = <<"+">>, expression_left=#int{int=5}, expression_right=#int{int=5}})),
    ?_assertEqual(0, ephp_context:solve(Ctx, #operation{type = <<"-">>, expression_left=#int{int=5}, expression_right=#int{int=5}})),
    ?_assertEqual(25, ephp_context:solve(Ctx, #operation{type = <<"*">>, expression_left=#int{int=5}, expression_right=#int{int=5}})),
    ?_assertEqual(1.0, ephp_context:solve(Ctx, #operation{type = <<"/">>, expression_left=#int{int=5}, expression_right=#int{int=5}})),
    ?_assertEqual(true, ephp_context:solve(Ctx, #operation{type = <<"<">>, expression_left=#int{int=1}, expression_right=#int{int=5}})),
    ?_assertEqual(false, ephp_context:solve(Ctx, #operation{type = <<">">>, expression_left=#int{int=1}, expression_right=#int{int=5}})),
    ?_assertEqual(7, ephp_context:solve(Ctx, #operation{type = <<"|">>, expression_left=#int{int=5}, expression_right=#int{int=2}})),
    ?_assertEqual(4, ephp_context:solve(Ctx, #operation{type = <<"^">>, expression_left=#int{int=5}, expression_right=#int{int=1}})),
    ?_assertEqual(4, ephp_context:solve(Ctx, #operation{type = <<"&">>, expression_left=#int{int=5}, expression_right=#int{int=6}}))
].

arith_mono_test(Ctx) ->
    Var = {variable,<<"a">>,[]}, [
    ?_assertEqual(ok, ephp_context:set(Ctx,Var,10)),
    ?_assertEqual(11, ephp_context:solve(Ctx, {pre_incr, Var})),
    ?_assertEqual(10, ephp_context:solve(Ctx, {pre_decr, Var})),
    ?_assertEqual(10, ephp_context:solve(Ctx, {post_incr, Var})),
    ?_assertEqual(11, ephp_context:solve(Ctx, {post_decr, Var})),
    ?_assertEqual(10, ephp_context:get(Ctx,Var))
].

global_test(Ctx) -> 
    Var = {variable,<<"a">>,[]},
    {ok,SubCtx} = ephp_context:generate_subcontext(Ctx), [
    ?_assertEqual(<<"hello">>, ephp_context:solve(Ctx, {assign, Var, {text, <<"hello">>}})),
    ?_assertEqual(null, ephp_context:solve(SubCtx, {global, Var})),
    ?_assertEqual(10, ephp_context:solve(SubCtx, {assign, Var, {int, 10}})),
    ?_assertEqual(10, ephp_context:get(SubCtx, {variable, <<"a">>, []})),
    ?_assertEqual(10, ephp_context:get(Ctx, {variable, <<"a">>, []}))
].

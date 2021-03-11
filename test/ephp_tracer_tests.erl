-module(ephp_tracer_tests).

-author('manuel@altenwald.com').

-compile([warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").

-include("ephp.hrl").

-define(CODE, <<"<?php\n\nprint 'hello\\n';\n">>).

cover_test() ->
    Self = self(),
    Output = fun(#stack_trace{} = ST) -> Self ! ST end,
    {ok, _} = ephp_tracer:start_link(Output),
    ephp_tracer:calls({function, <<"print">>}),
    {ok, Ctx} = ephp:context_new(),
    ok = ephp_config:set(<<"tracer.enable">>, true),
    ok = ephp:register_module(Ctx, ephp_lib_string),
    ?assertEqual({ok, false}, ephp:eval(Ctx, ?CODE)),
    ok = ephp_tracer:stop(),
    Ret = receive
              #stack_trace{function = <<"print">>} ->
                  true;
              Other ->
                  throw(Other)
          after 0 ->
              false
          end,
    ?assert(Ret),
    ok.

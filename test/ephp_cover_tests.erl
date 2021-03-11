-module(ephp_cover_tests).

-author('manuel@altenwald.com').

-compile([warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").

-include("ephp.hrl").

-define(CODE, <<"<?php\n\nprint 'hello\\n';\n">>).
-define(COVER,
        <<"<?xml version=\"1.0\"?><!--DOCTYPE coverage SYSTEM "
          "\"http://cobertura.sourceforge.net/xml/coverage-04.dtd\"-->"
          "<coverage timestamp=\"1474806235701\" line-rate=\"1.000000\""
          " lines-covered=\"0\" lines-valid=\"0\" branch-rate=\"0.0\""
          " branches-covered=\"0\" branches-valid=\"0\" complexity=\"0\""
          " version=\"1.9.4.1\"><sources><source>.</source></sources>"
          "<packages><package name=\"base\" line-rate=\"1.000000\" "
          "branch-rate=\"0\" complexity=\"0\"><classes>"
          "<class name=\"-\" filename=\"-\" line-rate=\"1.000000\" "
          "branch-rate=\"0.0\" complexity=\"0\"><methods/><lines>"
          "<line number=\"1\" hits=\"1\"/>"
          "<line number=\"3\" hits=\"1\"/>"
          "</lines></class></classes></package></packages></coverage>">>).

cover_test() ->
    {ok, Ctx} = ephp:context_new(),
    ok = ephp_config:set(<<"cover.enable">>, true),
    ok = ephp_config:set(<<"cover.output">>, <<"cobertura_test.xml">>),
    ?assert(ephp_cover:get_config()),
    ok = ephp:register_module(Ctx, ephp_lib_string),
    ?assertEqual({ok, false}, ephp:eval(Ctx, ?CODE)),
    ok = ephp:stop_cover(),
    ?assertEqual({ok, ?COVER}, file:read_file(<<"cobertura_test.xml">>)),
    ephp_config:set(<<"cover.enable">>, false),
    ok.

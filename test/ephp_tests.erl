-module(ephp_tests).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include_lib("eunit/include/eunit.hrl").
-include("ephp.hrl").

main_test() ->
    ?assertEqual(0, ephp:main(["test/code/test_empty.php"])).

main_file_not_found_test() ->
    ?assertEqual(2, ephp:main(["test/code/file_not_found"])).

main_error_test() ->
    ?assertEqual(1, ephp:main(["test/code/error.php"])).

main_other_error_test() ->
    ?assertEqual(3, ephp:main(["test/"])).

main_empty_test() ->
    ?assertEqual(1, ephp:main([])).

main_lint_test() ->
    ?assertEqual(0, ephp:main(["-l", "test/code/test_empty.php"])),
    ?assertEqual(1, ephp:main(["-l", "test/code/file_not_found"])),
    ?assertException(throw, {error, eparse, _Index, ?E_PARSE, _Data},
                     ephp:main(["-l", "test/code/error.php"])),
    ok.

main_dir_test() ->
    ?assertEqual(0, ephp:main(["-d", "test/code"])),
    ok.

main_run_test() ->
    ?assertEqual(0, ephp:main(["-r", "echo 'Hello world';"])),
    ok.

main_phpinfo_test() ->
    ephp:start(),
    application:set_env(ephp, sapi, <<"cgi">>),
    ?assertMatch(0, ephp:main(["-i"])),
    application:set_env(ephp, sapi, <<"cli">>),
    ?assertMatch(0, ephp:main(["-i"])),
    ok.

register_var_test() ->
    {ok, Ctx} = ephp:context_new(),
    ephp:register_var(Ctx, <<"a">>, 21),
    ?assertEqual({ok,{return,21}}, ephp:eval(Ctx, <<"<? return $a;">>)),
    ?assertEqual({error, badarg}, ephp:register_var(Ctx, <<"a">>, {})),
    ok.

run_empty_php_test() ->
    {ok, Ctx} = ephp:context_new(),
    ?assertEqual({ok, false}, ephp:eval(Ctx, <<>>)),
    ok.

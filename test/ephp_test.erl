-module(ephp_test).
-author('manuel@altenwald.com').
-compile([warnings_as_errors,export_all]).

-include_lib("eunit/include/eunit.hrl").

main_test() ->
    ?assertEqual(0, ephp:main(["../test/code/test_empty.php"])).

main_file_not_found_test() ->
    ?assertEqual(-2, ephp:main(["../test/code/file_not_found"])).

main_error_test() ->
    ?assertEqual(-1, ephp:main(["../test/code/error.php"])).

main_other_error_test() ->
    ?assertEqual(-3, ephp:main(["../test/"])).

main_empty_test() ->
    ?assertEqual(-1, ephp:main([])).

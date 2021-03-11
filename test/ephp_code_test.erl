-module(ephp_code_test).

-author('manuel@altenwald.com').

-compile([warnings_as_errors]).

-define(CODE_PATH, "test/code/").

-include_lib("eunit/include/eunit.hrl").

-include("ephp.hrl").

-define(ARGS, [<<"a">>, <<"b">>, <<"c">>]).

eval(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            AbsFilename = list_to_binary(filename:absname(Filename)),
            ephp_config:start_link(?PHP_INI_FILE),
            ephp_config:start_local(),
            {ok, Ctx} = ephp:context_new(AbsFilename),
            ephp:register_superglobals(Ctx, Filename, [Filename | ?ARGS]),
            {ok, Output} = ephp_output:start_link(Ctx, false),
            ephp_context:set_output_handler(Ctx, Output),
            try
                ephp:eval(AbsFilename, Ctx, Content)
            catch
                {ok, die} ->
                    ok
            end,
            Out = ephp_context:get_output(Ctx),
            ephp_context:destroy_all(Ctx),
            ephp_config:stop_local(),
            {ok, Out};
        Error ->
            Error
    end.

% OTP_RELEASE was included in OTP 21
-ifndef(OTP_RELEASE).

test_code(File) ->
    {timeout,
     15,
     {File,
      ?_assert(begin
                   try
                       run_test_code(File)
                   catch
                       Type:Reason ->
                           ?debugFmt("~n *** ERROR in ~s.php why: ~p~nreason:~n~p~n~p~n",
                                     [File, Type, Reason, erlang:get_stacktrace()]),
                           false
                   end
               end)}}.

-else.

test_code(File) ->
    {timeout,
     15,
     {File,
      ?_assert(begin
                   try
                       run_test_code(File)
                   catch
                       Type:Reason:Stacktrace ->
                           ?debugFmt("~n *** ERROR in ~s.php why: ~p~nreason:~n~p~n~p~n",
                                     [File, Type, Reason, Stacktrace]),
                           false
                   end
               end)}}.

-endif.

run_test_code(File) ->
    {ok, OutCode} = eval(?CODE_PATH ++ File ++ ".php"),
    {ok, OutFileRaw} = file:read_file(?CODE_PATH ++ File ++ ".out"),
    {ok, CWD} = file:get_cwd(),
    OutFileRaw2 = binary:replace(OutFileRaw, <<"{{CWD}}">>, list_to_binary(CWD), [global]),
    Match = <<"{{CWDLEN\\s?\\+\\s?(\\d+)}}">>,
    Opts = [global, {capture, all, binary}],
    CWDLen = length(CWD),
    OutFile =
        case re:run(OutFileRaw2, Match, Opts) of
            {match, Matches} ->
                lists:foldl(fun([M, Num], Out) ->
                               N = CWDLen + binary_to_integer(Num),
                               binary:replace(Out, M, integer_to_binary(N))
                            end,
                            OutFileRaw2,
                            Matches);
            nomatch ->
                OutFileRaw2
        end,
    ?assertEqual(OutFile, iolist_to_binary(OutCode)),
    true.

code_to_test_() ->
    ephp:start(),
    Codes =
        case os:getenv("CODE_TEST") of
            false ->
                {ok, Files} = file:list_dir(?CODE_PATH),
                [filename:rootname(File) || File <- Files, filename:extension(File) =:= ".out"];
            Filename ->
                [Filename]
        end,
    [test_code(X) || X <- Codes].

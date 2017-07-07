-module(ephp_request_test).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-define(CODE_PATH, "test/requests/").

-include_lib("eunit/include/eunit.hrl").
-include("ephp.hrl").

eval(Filename) ->
    case file:read_file(Filename) of
    {ok, Content} ->
        AbsFilename = list_to_binary(filename:absname(Filename)),
        ephp_config:start_link(?PHP_INI_FILE),
        ephp_config:start_local(),
        {ok, Ctx} = ephp:context_new(AbsFilename),
        {ok, Output} = ephp_output:start_link(Ctx, false),
        ephp_context:set_output_handler(Ctx, Output),

        % add linked array
        ReqArray = ephp_array:new(ephp_request_dets, handle_array, [make_ref()]),
        ephp_context:set(Ctx, #variable{name = <<"_REQUEST">>}, ReqArray),

        ephp:eval(AbsFilename, Ctx, Content),
        Out = ephp_context:get_output(Ctx),
        ephp_context:destroy_all(Ctx),
        ephp_config:stop_local(),
        {ok, Out};
    Error ->
        Error
    end.

test_code(File) ->
    {File, ?_assert(begin
        try
            {ok, OutCode} = eval(?CODE_PATH ++ File ++ ".php"),
            {ok, OutFileRaw} = file:read_file(?CODE_PATH ++ File ++ ".out"),
            {ok, CWD} = file:get_cwd(),
            OutFile = binary:replace(OutFileRaw, <<"{{CWD}}">>,
                list_to_binary(CWD), [global]),
            ?assertEqual(OutFile, iolist_to_binary(OutCode)),
            true
        catch Type:Reason ->
            ?debugFmt("~n\t*** ERROR in ~s.php why: ~p; reason: ~p~n~p~n",
                [File, Type, Reason, erlang:get_stacktrace()]),
            false
        end
    end)}.

code_to_test_() ->
    ephp:start(),
    {ok, Files} = file:list_dir(?CODE_PATH),
    Codes = [ filename:rootname(File) || File <- Files,
        filename:extension(File) =:= ".out" ],
    lists:map(fun(X) -> test_code(X) end, Codes).

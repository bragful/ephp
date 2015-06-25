-module(ephp_code_test).
-author('manuel@altenwald.com').
-compile([warnings_as_errors, export_all]).

-define(CODE_PATH, "../test/code/").

-include_lib("eunit/include/eunit.hrl").

eval(Filename) ->
    case file:read_file(Filename) of
    {ok, Content} ->
        AbsFilename = list_to_binary(filename:absname(Filename)),
        {ok, Ctx} = ephp:context_new(AbsFilename),
        {ok, Output} = ephp_output:start_link(false),
        ephp_context:set_output_handler(Ctx, Output),
        {ok, Ret} = ephp:eval(AbsFilename, Ctx, Content),
        Out = ephp_context:get_output(Ctx),
        ephp_context:destroy_all(Ctx), 
        {ok, Out, Ret};
    Error ->
        Error
    end.

test_code(File) ->
    ?_assert(begin
        try
            {ok, OutCode, _Ret} = eval(?CODE_PATH ++ File ++ ".php"),
            {ok, OutFileRaw} = file:read_file(?CODE_PATH ++ File ++ ".out"),
            {ok, CWD} = file:get_cwd(),
            OutFile = binary:replace(OutFileRaw, <<"{{CWD}}">>,
                list_to_binary(CWD ++ "/.."), [global]),
            ?assertEqual(iolist_to_binary(OutCode), OutFile),
            true
        catch Type:Reason ->
            ?debugFmt("~n\t*** ERROR in ~s.php why: ~p; reason: ~p~n~p~n",
                [File, Type, Reason, erlang:get_stacktrace()]),
            false
        end
    end).

code_to_test_() ->
    {ok, Files} = file:list_dir(?CODE_PATH),
    Codes = [ filename:rootname(File) || File <- Files,
        filename:extension(File) =:= ".out" ],
    lists:map(fun(X) -> test_code(X) end, Codes).

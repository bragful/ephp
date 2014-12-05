-module(ephp_code_test).
-compile(export_all).

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
        {ok, ephp_context:get_output(Ctx), Ret};
    Error ->
        Error
    end.

test_code(File) ->
    ?_assert(begin
        try
            {ok, OutCode, _Ret} = eval(?CODE_PATH ++ File ++ ".php"),
            {ok, OutFile} = file:read_file(?CODE_PATH ++ File ++ ".out"),
            ?assertEqual(iolist_to_binary(OutCode), OutFile),
            true
        catch Type:Reason ->
            ?debugFmt("~n\t*** ERROR in ~s.php why: ~p; reason: ~p~n",
                [File, Type, Reason]),
            false
        end
    end).

code_to_test_() ->
    Codes = [
        "empty",
        "array_01",
        "strings_01",
        "template_01",
        "text_op"
    ],
    lists:map(fun(X) -> test_code(X) end, Codes).

%% @doc The ephp module is in charge to give an easy way to create the context
%%      and other actions to be performed from the project where ephp is
%%      included mainly to run the PHP code.
%%
%%      The easy way to use is:
%%
%%      <pre lang="erlang"><![CDATA[
%%      {ok, Ctx} = ephp:context_new(),
%%      PHP = "<? $a = 5 * 23; ?>Result for $a = <?=$a?>",
%%      {ok, Text} = ephp:eval(Ctx, PHP).
%%      ]]></pre>
%%
%%      This module is in use for PHP script, contains the `main/1' function
%%      to run from console.
%% @end
-module(ephp).

-author('manuel@altenwald.com').

-export([context_new/0, context_new/1, register_var/3, register_func/6, register_func/7,
         register_module/2, eval/2, eval/3, start/0, main/1, register_superglobals/2,
         register_superglobals/3, get_var/2, get_var/3, naive_eval/3]).

              %% for escriptize

-ifdef(TEST).

-export([stop_cover/0]).

-endif.

-define(ERRORLEVEL_OK, 0).
-define(ERRORLEVEL_1, 1).
-define(ERRORLEVEL_2, 2).
-define(ERRORLEVEL_3, 3).
% built-in modules
-define(MODULES,
        [ephp_lib_date,
         ephp_lib_vars,
         ephp_lib_math,
         ephp_lib_misc,
         ephp_lib_ob,
         ephp_lib_control,
         ephp_lib_array,
         ephp_lib_string,
         ephp_lib_file,
         ephp_lib_func,
         ephp_lib_info,
         ephp_lib_class,
         ephp_lib_error,
         ephp_lib_pcre,
         ephp_lib_spl,
         ephp_lib_exec]).

-export_type([context_id/0, vars_id/0, output_id/0, funcs_id/0, classes_id/0,
              objects_id/0, consts_id/0, includes_id/0, shutdown_id/0, errors_id/0]).

-type context_id() :: reference().
-type vars_id() :: reference().
-type output_id() :: reference().
-type funcs_id() :: reference().
-type classes_id() :: reference().
-type objects_id() :: reference().
-type consts_id() :: reference().
-type includes_id() :: reference().
-type shutdown_id() :: reference().
-type errors_id() :: reference().

-include("ephp.hrl").
-include("ephp_array.hrl").

-spec context_new() -> {ok, context_id()}.
%% @doc creates a new context using `-' as script name.
context_new() ->
    context_new(<<"-">>).

-spec context_new(Filename :: binary()) -> {ok, context_id()}.
%% @doc creates a new context passing `Filename' as param.
context_new(Filename) ->
    Modules = application:get_env(ephp, modules, []),
    {ok, Ctx} = ephp_context:start_link(),
    {ok, PathStr} = file:get_cwd(),
    ephp_stream:set_initial_path(list_to_binary(PathStr)),
    [register_module(Ctx, Module) || Module <- Modules],
    ephp_context:set_active_file(Ctx, Filename),
    {ok, Ctx}.

-type values() :: integer() | binary() | float() | ephp_array:ephp_array().

-spec register_var(Ctx :: context_id(), Var :: binary(), Value :: values()) ->
                      ok | {error, reason()}.
%% @doc register a variable with a value in the context passed as param.
register_var(Ctx, Var, Value)
    when is_reference(Ctx)
         andalso (is_integer(Value)
                  orelse is_float(Value)
                  orelse is_binary(Value)
                  orelse ?IS_ARRAY(Value)) ->
    ephp_context:set(Ctx, #variable{name = Var}, Value);
register_var(_Ctx, _Var, _Value) ->
    {error, badarg}.

-spec register_func(context_id(),
                    ephp_ns:namespace(),
                    PHPName :: binary(),
                    module(),
                    Fun :: atom(),
                    PackArgs :: boolean(),
                    ephp_lib:validation_args()) ->
                       ok | {error, reason()}.
%% @doc register function in a context passed as a param. The params to be
%%      sent are the PHP function name, the module, function name and args
%%      in the Erlang side.
%%
%%      Other param is about if the params should be packed or not. That means
%%      the args could be sent one by one or as only one in an array.
%% @end
register_func(Ctx, NS, PHPName, Module, Fun, PackArgs, Args) ->
    ephp_context:register_func(Ctx, NS, PHPName, Module, Fun, PackArgs, Args).

-spec register_func(context_id(),
                    PHPName :: binary(),
                    module(),
                    Fun :: atom(),
                    PackArgs :: boolean(),
                    ephp_lib:validation_args()) ->
                       ok | {error, reason()}.
%% @doc register function in a context passed as a param. The params to be
%%      sent are the PHP function name, the module, function name and args
%%      in the Erlang side.
%%
%%      Other param is about if the params should be packed or not. That means
%%      the args could be sent one by one or as only one in an array.
%% @end
register_func(Ctx, PHPName, Module, Fun, PackArgs, Args) ->
    ephp_context:register_func(Ctx, [], PHPName, Module, Fun, PackArgs, Args).

-spec register_module(context_id(), module()) -> ok.
%% @doc register a module.
%% @see ephp_func
register_module(Ctx, Module) ->
    ephp_lib:register(Ctx, Module).

-spec get_var(context_id(), binary()) -> mixed().
%% @doc get variable from context.
get_var(Context, VarName) ->
    get_var(Context, VarName, []).

-spec get_var(context_id(), binary(), [array_index() | object_index() | class_index()]) -> mixed().
%% @doc get variable from context giving also the indexes.
get_var(Context, VarName, Idx) ->
    ephp_context:get(Context, #variable{name = VarName, idx = Idx}).

-type eval_return() ::
    {ok, Result :: ephp_interpr:flow_status()} |
    {error, reason(), line(), File :: binary(), ephp_error:error_level(), Data :: any()}.

-spec eval(context_id(), PHP :: string() | binary()) -> eval_return().
%% @doc eval PHP code in a context passed as params.
eval(Context, PHP) ->
    eval(<<"-">>, Context, PHP).

-spec eval(Filename :: binary(), context_id(), PHP :: string() | binary() | [term()]) ->
              eval_return().
%% @equiv eval/2
%% @doc adds the `Filename' to configure properly the `__FILE__' and `__DIR__'
%%      constants and evaluates the code for the third parameter. This parameter
%%      could contents a binary text with PHP code or a parsed PHP content.
%% @end
eval(Filename, Context, PHP) when is_binary(PHP) ->
    try
        Compiled = ephp_parser:parse(Filename, PHP),
        eval(Filename, Context, Compiled)
    catch
        {error, ErrorName, Line, ErrorLevel, Data} ->
            ephp_error:handle_error(Context, {error, ErrorName, Line, Filename, ErrorLevel, Data}),
            {error, ErrorName, Line, Filename, ErrorLevel, Data}
    end;
eval(Filename, Context, Compiled) ->
    Cover = ephp_cover:get_config(),
    ok = ephp_cover:init_file(Cover, Filename, Compiled),
    case catch ephp_interpr:process(Context, Compiled, Cover) of
        {ok, Return} ->
            maybe_die(fun() -> ephp_shutdown:shutdown(Context) end),
            {ok, Return};
        die ->
            maybe_die(fun() -> ephp_shutdown:shutdown(Context) end),
            {ok, undefined};
        {error, Reason, Index, Level, Data} ->
            File = ephp_context:get_active_file(Context),
            Error = {error, Reason, Index, File, Level, Data},
            maybe_die(fun() -> ephp_error:handle_error(Context, Error) end),
            maybe_die(fun() -> ephp_shutdown:shutdown(Context) end),
            Error
    end.

-spec naive_eval(Filename :: binary(), context_id(), PHP :: string() | binary() | [term()]) ->
              eval_return().
%% @doc adds the `Filename' to configure properly the `__FILE__' and `__DIR__'
%%      constants and evaluates the code for the third parameter. This parameter
%%      could contents a binary text with PHP code or a parsed PHP content.
%%      In difference with `eval/3` it's not shutting down so makes possible to have
%%      access to the information inside of the context after running a piece of code.
%%      **CAUTION**: keep in mind it could leak your memory if you are not shutting down.
%%      Also, it's not using cover.
%% @end
naive_eval(Filename, Context, PHP) when is_binary(PHP) ->
    try
        Compiled = ephp_parser:parse(Filename, PHP),
        naive_eval(Filename, Context, Compiled)
    catch
        {error, ErrorName, Line, ErrorLevel, Data} ->
            ephp_error:handle_error(Context, {error, ErrorName, Line, Filename, ErrorLevel, Data}),
            {error, ErrorName, Line, Filename, ErrorLevel, Data}
    end;
naive_eval(_Filename, Context, Compiled) ->
    case catch ephp_interpr:process(Context, Compiled, false) of
        {ok, _} = OkTuple ->
            OkTuple;
        die ->
            {ok, undefined};
        {error, Reason, Index, Level, Data} ->
            File = ephp_context:get_active_file(Context),
            {error, Reason, Index, File, Level, Data}
    end.

maybe_die(Fun) ->
    try
        Fun()
    catch
        {ok, die} ->
            ok
    end.

-spec opt_spec_list() -> [getopt:option_spec()].
%% @hidden
opt_spec_list() ->
    [{help, $h, "help", undefined, "This help information."},
     {dir, $d, "dir", string, "Check directory for missing implemented PHP functions."},
     {check, $l, undefined, string, "Check PHP code syntax for specific PHP file."},
     {parse, $p, undefined, string, "Show parsing PHP code for specific PHP file."},
     {info, $i, "info", undefined, "Show PHP info."},
     {run, $r, undefined, string, "Run PHP code."},
     {file, undefined, undefined, string, "PHP code to be run."}].

-spec start() -> ok.
%% @doc function to ensure all of the applications and the base configuration
%%      is set properly before use ephp.
%% @end
start() ->
    application:start(ezic),
    application:start(zucchini),
    application:start(ephp),
    application:start(getopt),
    application:set_env(ephp, modules, ?MODULES),
    ok.

-type quit_return() :: no_return().

-ifndef(TEST).

-spec quit(non_neg_integer() | abort | [char()]) -> no_return().
%% @hidden
quit(Code) ->
    erlang:halt(Code).

-else.

%% @hidden
-spec quit(non_neg_integer() | abort | [char()]) -> non_neg_integer() | abort | [char()].
quit(Code) ->
    Code.

-endif.

-spec usage() -> ok.
%% @doc shows the help information.
usage() ->
    ScriptName =
        try
            escript:script_name()
        catch
            error:{badarg, []} ->
                "ephp"
        end,
    getopt:usage(opt_spec_list(), ScriptName),
    ok.

-spec main(Args :: [string()]) -> quit_return().
%% @doc called from script passing the name of the filename to be run or
%%      nothing to show the help message.
%% @end
main(Args) ->
    case getopt:parse(opt_spec_list(), Args) of
        {ok, {Opts, _OtherArgs}} when Opts =/= [] ->
            quit(main(Opts, Args));
        _Else ->
            usage(),
            quit(?ERRORLEVEL_1)
    end.

-spec main(Opts :: [getopt:option()], Args :: [string()]) -> integer().
%% @hidden
main([{dir, Dir} | _], _RawArgs) ->
    start(),
    {Funcs, TotalOk, Total} = parse_subdirs(Dir),
    {ok, Context} = context_new(),
    FTotal = length(Funcs),
    FTotalOk =
        lists:foldl(fun(Func, FuncOK) ->
                       case ephp_context:is_defined_function(Context, Func) of
                           true ->
                               io:format("~s: OK~n", [Func]),
                               FuncOK + 1;
                           false ->
                               io:format("~s: NOT FOUND~n", [Func]),
                               FuncOK
                       end
                    end,
                    0,
                    Funcs),
    io:format("~nFunctions FOUND=~p Total=~p (~p%) / Code OK=~p Total=~p (~p%)~n",
              [FTotalOk,
               FTotal,
               ephp_data:ceiling(FTotalOk * 100 / FTotal),
               TotalOk,
               Total,
               ephp_data:ceiling(TotalOk * 100 / Total)]),
    ?ERRORLEVEL_OK;
main([{check, File} | _], _RawArgs) ->
    maybe_badmatch(fun() ->
                      ephp_parser:file(File),
                      io:format("No syntax errors detected in: ~s~n", [File]),
                      ?ERRORLEVEL_OK
                   end,
                   File);
main([{parse, File} | _], _RawArgs) ->
    maybe_badmatch(fun() ->
                      io:format("parsing =>~n~p~n---~n", [ephp_parser:file(File)]),
                      ?ERRORLEVEL_OK
                   end,
                   File);
main([info | _], _RawArgs) ->
    start(),
    Content = <<"<?php phpinfo();">>,
    ephp_config:start_link(?PHP_INI_FILE),
    {ok, Ctx} = context_new(<<"-">>),
    register_superglobals(Ctx, []),
    {ok, _} = eval(<<"-">>, Ctx, Content),
    output_and_close(Ctx),
    ?ERRORLEVEL_OK;
main([{run, Code} | _], _RawArgs) ->
    start(),
    Content = list_to_binary("<?php " ++ Code),
    ephp_config:start_link(?PHP_INI_FILE),
    {ok, Ctx} = context_new(<<"-">>),
    register_superglobals(Ctx, []),
    case eval(<<"-">>, Ctx, Content) of
        {ok, _Return} ->
            output_and_close(Ctx),
            ?ERRORLEVEL_OK;
        {error, _Reason, _Index, _File, _Level, _Data} ->
            ?ERRORLEVEL_1
    end;
main([help | _], _) ->
    usage(),
    ?ERRORLEVEL_OK;
main([{file, Filename} | _], RawArgs) ->
    start(),
    case file:read_file(Filename) of
        {ok, Content} ->
            start_profiling(),
            start_cover(),
            AbsFilename = list_to_binary(filename:absname(Filename)),
            ephp_config:start_link(?PHP_INI_FILE),
            {ok, Ctx} = context_new(AbsFilename),
            register_superglobals(Ctx, Filename, RawArgs),
            case eval(AbsFilename, Ctx, Content) of
                {ok, _Return} ->
                    output_and_close(Ctx),
                    stop_profiling(),
                    stop_cover(),
                    ?ERRORLEVEL_OK;
                {error, _Reason, _Index, _File, _Level, _Data} ->
                    stop_profiling(),
                    ?ERRORLEVEL_1
            end;
        {error, enoent} ->
            io:format("File not found: ~s~n", [Filename]),
            ?ERRORLEVEL_2;
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            ?ERRORLEVEL_3
    end;
main(_, _) ->
    usage(),
    ?ERRORLEVEL_1.

-spec maybe_badmatch(fun(() -> integer()), string()) -> integer().
%% @hidden
maybe_badmatch(Fun, File) ->
    try
        Fun()
    catch
        error:{badmatch, {error, enoent}} ->
            io:format("Could not open input file: ~s~n", [File]),
            ?ERRORLEVEL_1
    end.

-spec output_and_close(context_id()) -> ok.
%% @hidden
output_and_close(Ctx) ->
    Result = ephp_context:get_output(Ctx),
    io:format("~s", [Result]),
    ephp_context:destroy_all(Ctx),
    ok.

-ifdef(PROFILING).

-spec start_profiling() -> ok.
start_profiling() ->
    eprof:start(),
    eprof:start_profiling([self()]),
    ok.

-spec stop_profiling() -> ok.
stop_profiling() ->
    eprof:stop_profiling(),
    eprof:analyze(total),
    ok.

-else.

-spec start_profiling() -> ok.
start_profiling() ->
    ok.

-spec stop_profiling() -> ok.
stop_profiling() ->
    ok.

-endif.

-spec start_cover() -> ok.
%% @doc starts the cover system.
start_cover() ->
    ephp_cover:start_link().

-spec stop_cover() -> ok.
%% @doc stops the cover system.
stop_cover() ->
    case ephp_cover:get_config() of
        true ->
            ephp_cover:dump();
        false ->
            ok
    end.

-spec register_superglobals(context_id(), [string()]) -> ok.
%% @doc register the superglobals variables in the context passed as param.
%%      by default this function set the filename as "-".
%% @end
register_superglobals(Ctx, RawArgs) ->
    register_superglobals(Ctx, "-", RawArgs).

-spec register_superglobals(context_id(), string(), [string()]) -> ok.
%% @doc register the superglobals variables in the context passed as param.
register_superglobals(Ctx, Filename, RawArgs) ->
    Args = [ephp_data:to_bin(RawArg) || RawArg <- RawArgs],
    ArrayArgs = ephp_array:from_list(Args),
    ArrayServer =
        ephp_array:from_list([%% TODO: add the rest of _SERVER vars
                              {<<"argc">>, ephp_array:size(ArrayArgs)},
                              {<<"argv">>, ArrayArgs},
                              {<<"PHP_SELF">>, list_to_binary(Filename)}]),
    ArrayNew = ephp_array:new(),
    ephp_context:set_bulk(Ctx,
                          [{<<"_SERVER">>, ArrayServer},
                           {<<"argc">>, ephp_array:size(ArrayArgs)},
                           {<<"argv">>, ArrayArgs},
                           {<<"_GET">>, ArrayNew},
                           {<<"_POST">>, ArrayNew},
                           {<<"_FILES">>, ArrayNew},
                           {<<"_COOKIE">>, ArrayNew},
                           {<<"_SESSION">>, ArrayNew},
                           {<<"_REQUEST">>, ArrayNew},
                           {<<"_ENV">>, ArrayNew}]),
    ok.

-spec get_use_funcs([tuple()], [binary()]) -> [binary()].
%% @doc get the name of the functions used in the code (no methods).
get_use_funcs([#call{type = normal, name = Name} = Tuple | Tuples], R)
    when is_binary(Name) ->
    get_use_funcs(erlang:tuple_to_list(Tuple), [])
    ++ get_use_funcs(Tuples, [ephp_string:to_lower(Name) | R]);
get_use_funcs([{object, _, _} | Tuples], R) ->
    get_use_funcs(Tuples, R);
get_use_funcs([Tuple | Tuples], R) when is_tuple(Tuple) ->
    get_use_funcs(erlang:tuple_to_list(Tuple), []) ++ get_use_funcs(Tuples, R);
get_use_funcs([List | Rest], R) when is_list(List) ->
    get_use_funcs(List, []) ++ get_use_funcs(Rest, R);
get_use_funcs([_Other | Rest], R) ->
    get_use_funcs(Rest, R);
get_use_funcs([], R) ->
    ordsets:from_list(R).

-spec get_defined_funcs([tuple()], [binary()]) -> [binary()].
%% @doc get the name of the functions used in the code (no methods).
get_defined_funcs([#function{name = Name} = Tuple | Tuples], R) ->
    get_defined_funcs(erlang:tuple_to_list(Tuple), [])
    ++ get_defined_funcs(Tuples, [ephp_string:to_lower(Name) | R]);
get_defined_funcs([Tuple | Tuples], R) when is_tuple(Tuple) ->
    get_defined_funcs(erlang:tuple_to_list(Tuple), []) ++ get_defined_funcs(Tuples, R);
get_defined_funcs([List | Rest], R) when is_list(List) ->
    get_defined_funcs(List, []) ++ get_defined_funcs(Rest, R);
get_defined_funcs([_Other | Rest], R) ->
    get_defined_funcs(Rest, R);
get_defined_funcs([], R) ->
    ordsets:from_list(R).

-spec parse_subdirs(string()) -> {[binary()], pos_integer(), pos_integer()}.
%% @doc given a directory it retrieves the functions in use for all of the
%%      files of that directory, the number of files ok (parsing correctly)
%%      and the number of files in the directory (with the php extension).
%% @end
parse_subdirs(Dir) ->
    Files = filelib:wildcard(Dir ++ "/**/*.php"),
    Total = length(Files),
    {Funcs, DefFuncs, TotalOk, _} =
        lists:foldl(fun(File, {Funcs, DefFuncs, I, J}) ->
                       Porc = ephp_data:ceiling(I * 100 / Total),
                       io:format("(~3..0b/~b) ~2..0b% file => ~s", [I + 1, Total, Porc, File]),
                       try
                           P = ephp_parser:file(File),
                           io:format(": OK~n", []),
                           {ordsets:from_list(get_use_funcs(P, Funcs)),
                            ordsets:from_list(get_defined_funcs(P, DefFuncs)),
                            I + 1,
                            J + 1}
                       catch
                           _:_ ->
                               io:format(": FAIL~n", []),
                               {Funcs, DefFuncs, I, J + 1}
                       end
                    end,
                    {[], [], 0, 0},
                    Files),
    {Funcs -- DefFuncs, TotalOk, Total}.

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
-compile([warnings_as_errors]).

-export([
    context_new/0,
    context_new/1,
    register_var/3,
    register_func/6,
    register_module/2,
    eval/2,
    eval/3,

    start/0,
    main/1,   %% for escriptize
    register_superglobals/2
]).

-ifdef(TEST).
-export([stop_cover/0]).
-endif.

-export_type([
    context_id/0,
    vars_id/0,
    output_id/0,
    funcs_id/0,
    classes_id/0,
    objects_id/0,
    consts_id/0,
    includes_id/0,
    shutdown_id/0,
    errors_id/0
]).

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

-spec context_new() -> {ok, context()}.
%% @doc creates a new context using `-' as script name.
context_new() ->
    context_new(<<"-">>).

-spec context_new(Filename :: binary()) -> {ok, context()}.
%% @doc creates a new context passing `Filename' as param.
context_new(Filename) ->
    Modules = application:get_env(ephp, modules, []),
    {ok, Ctx} = ephp_context:start_link(),
    [ register_module(Ctx, Module) || Module <- Modules ],
    ephp_context:set_active_file(Ctx, Filename),
    {ok, Ctx}.

-type values() :: integer() | binary() | float() | ephp_array().

-spec register_var(Ctx :: context(), Var :: binary(), Value :: values()) ->
    ok | {error, reason()}.
%% @doc register a variable with a value in the context passed as param.
register_var(Ctx, Var, Value) when
        is_reference(Ctx) andalso
        (is_integer(Value) orelse
        is_float(Value) orelse
        is_binary(Value) orelse
        ?IS_ARRAY(Value)) ->
    ephp_context:set(Ctx, #variable{name=Var}, Value);

register_var(_Ctx, _Var, _Value) ->
    {error, badarg}.

-spec register_func(context(), PHPName :: binary(), module(), Fun :: atom(),
                    PackArgs :: boolean(),
                    Args :: ephp_func:validation_args()
                   ) -> ok | {error, reason()}.
%% @doc register function in a context passed as a param. The params to be
%%      sent are the PHP function name, the module, function name and args
%%      in the Erlang side.
%%
%%      Other param is about if the params should be packed or not. That means
%%      the args could be sent one by one or as only one in an array.
%% @end
register_func(Ctx, PHPName, Module, Fun, PackArgs, Args) ->
    ephp_context:register_func(Ctx, PHPName, Module, Fun, PackArgs, Args).

-spec register_module(context(), module()) -> ok.
%% @doc register a module.
%% @see ephp_func
register_module(Ctx, Module) ->
    ephp_config:module_init(Module),
    case erlang:function_exported(Module, handle_error, 3) of
        true -> ephp_error:add_message_handler(Ctx, Module);
        false -> ok
    end,
    ClassRef = ephp_context:get_classes(Ctx),
    case erlang:function_exported(Module, get_classes, 0) of
        true ->
            Classes = Module:get_classes(),
            ephp_class:register_classes(ClassRef, Ctx, Classes);
        false ->
            ok
    end,
    lists:foreach(fun
        ({Func, Opts}) ->
            PackArgs = proplists:get_value(pack_args, Opts, false),
            Name = proplists:get_value(alias, Opts, atom_to_binary(Func, utf8)),
            Args = proplists:get_value(args, Opts),
            register_func(Ctx, Name, Module, Func, PackArgs, Args);
        (Func) ->
            Name = atom_to_binary(Func, utf8),
            register_func(Ctx, Name, Module, Func, false, undefined)
    end, Module:init_func()).

-spec eval(context(), PHP :: string() | binary()) ->
    {ok, Result :: binary()} | {error, Reason :: reason()} |
    {error,{Code::binary(), Line::integer(), Col::integer()}}.
%% @doc eval PHP code in a context passed as params.
eval(Context, PHP) ->
    eval(<<"-">>, Context, PHP).

-spec eval(Filename :: binary(), context(),
           PHP :: string() | binary() | [term()]) ->
    {ok, Result :: binary()} |
    {error, reason(), line(), File::binary(), error_level(), Data::any()}.
%% @equiv eval/2
%% @doc adds the `Filename' to configure properly the `__FILE__' and `__DIR__'
%%      constants and evaluates the code for the third parameter. This parameter
%%      could contents a binary text with PHP code or a parsed PHP content.
%% @end
eval(Filename, Context, PHP) when is_binary(PHP) ->
    case catch ephp_parser:parse(PHP) of
        {error, ErrorName, Line, ErrorLevel, Data} ->
            ephp_error:handle_error(Context, {error, ErrorName, Line,
                Filename, ErrorLevel, Data}),
            {error, ErrorName, Line, Filename, ErrorLevel, Data};
        Compiled ->
            eval(Filename, Context, Compiled)
    end;

eval(Filename, Context, Compiled) ->
    Cover = ephp_cover:get_config(),
    ok = ephp_cover:init_file(Cover, Filename, Compiled),
    case catch ephp_interpr:process(Context, Compiled, Cover) of
        {ok, Return} ->
            try
                ephp_shutdown:shutdown(Context)
            catch
                throw:{ok, die} -> ok
            end,
            {ok, Return};
        die ->
            try
                ephp_shutdown:shutdown(Context)
            catch
                throw:{ok, die} -> ok
            end,
            {ok, undefined};
        {error, Reason, Index, Level, Data} ->
            File = ephp_context:get_active_file(Context),
            Error = {error, Reason, Index, File, Level, Data},
            try
                ephp_error:handle_error(Context, Error)
            catch
                throw:{ok, die} -> ok
            end,
            ephp_shutdown:shutdown(Context),
            Error
    end.

-spec start() -> ok.
%% @doc function to ensure all of the applications and the base configuration
%%      is set properly before use ephp.
%% @end
start() ->
    application:start(ezic),
    application:start(zucchini),
    application:start(ephp),
    application:set_env(ephp, modules, ?MODULES),
    ok.

-spec main(Args :: [string()]) -> integer().
%% @doc called from script passing the name of the filename to be run or
%%      nothing to show the help message.
%% @end
main(["-d", Dir]) ->
    start(),
    {Funcs, TotalOk, Total} = parse_subdirs(Dir),
    {ok, Context} = context_new(),
    FTotal = length(Funcs),
    FTotalOk = lists:foldl(fun(Func, FuncOK) ->
        case ephp_context:is_defined_function(Context, Func) of
            true ->
                io:format("~s: OK~n", [Func]),
                FuncOK + 1;
            false ->
                io:format("~s: NOT FOUND~n", [Func]),
                FuncOK
        end
    end, 0, Funcs),
    io:format("~nFunctions FOUND=~p Total=~p (~p%) / Code OK=~p Total=~p (~p%)~n",
              [FTotalOk, FTotal, ephp_data:ceiling(FTotalOk * 100 / FTotal),
               TotalOk, Total, ephp_data:ceiling(TotalOk * 100 / Total)]),
    quit(0);

main(["-l", File]) ->
    try
        ephp_parser:file(File),
        io:format("No syntax errors detected in: ~s~n", [File]),
        quit(0)
    catch
        error:{badmatch, {error, enoent}} ->
            io:format("Could not open input file: ~s~n", [File]),
            quit(1)
    end;

main(["-p", File]) ->
    try
        io:format("parsing =>~n~p~n---~n", [ephp_parser:file(File)]),
        quit(0)
    catch
        error:{badmatch, {error, enoent}} ->
            io:format("Could not open input file: ~s~n", [File]),
            quit(1)
    end;

main([Filename|_] = RawArgs) ->
    start(),
    case file:read_file(Filename) of
    {ok, Content} ->
        start_profiling(),
        start_cover(),
        AbsFilename = list_to_binary(filename:absname(Filename)),
        ephp_config:start_link(?PHP_INI_FILE),
        {ok, Ctx} = context_new(AbsFilename),
        register_superglobals(Ctx, RawArgs),
        case eval(AbsFilename, Ctx, Content) of
            {ok, _Return} ->
                Result = ephp_context:get_output(Ctx),
                io:format("~s", [Result]),
                ephp_context:destroy_all(Ctx),
                stop_profiling(),
                stop_cover(),
                quit(0);
            {error, _Reason, _Index, _File, _Level, _Data} ->
                stop_profiling(),
                quit(1)
        end;
    {error, enoent} ->
        io:format("File not found: ~s~n", [Filename]),
        quit(2);
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason]),
        quit(3)
    end;

main(_) ->
    io:format("Usage: ephp <file.php>~n", []),
    quit(1).

-ifndef(TEST).
-spec quit(integer()) -> no_return().
%% @hidden
quit(Code) ->
    erlang:halt(Code).
-else.
-spec quit(integer()) -> integer().
%% @hidden
quit(Code) ->
    Code.
-endif.

-ifdef(PROFILING).

-spec start_profiling() -> ok.
%% @doc starts the profiling functions to gather information using eprof.
start_profiling() ->
    eprof:start(),
    eprof:start_profiling([self()]),
    ok.

-spec stop_profiling() -> ok.
%% @doc stops the profiling system and send the analysis to the standard
%%      output.
%% @end
stop_profiling() ->
    eprof:stop_profiling(),
    eprof:analyze(total),
    ok.

-else.

-spec start_profiling() -> ok.
%% @hidden
start_profiling() ->
    ok.

-spec stop_profiling() -> ok.
%% @hidden
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
        true -> ephp_cover:dump();
        false -> ok
    end.


-spec register_superglobals(context(), [string()]) -> ok.
%% @doc register the superglobals variables in the context passed as param.
register_superglobals(Ctx, [Filename|_] = RawArgs) when is_list(Filename) ->
    Args = [ ephp_data:to_bin(RawArg) || RawArg <- RawArgs ],
    ArrayArgs = ephp_array:from_list(Args),
    Server = [
        %% TODO: add the rest of _SERVER vars
        {<<"argc">>, ephp_array:size(ArrayArgs)},
        {<<"argv">>, ArrayArgs},
        {<<"PHP_SELF">>, list_to_binary(Filename)}
    ],
    ephp_context:set(Ctx, #variable{name = <<"_SERVER">>},
                     ephp_array:from_list(Server)),
    SuperGlobals = [
        <<"_GET">>,
        <<"_POST">>,
        <<"_FILES">>,
        <<"_COOKIE">>,
        <<"_SESSION">>,
        <<"_REQUEST">>,
        <<"_ENV">>
    ],
    lists:foreach(fun(Global) ->
        ephp_context:set(Ctx, #variable{name = Global}, ephp_array:new())
    end, SuperGlobals),
    ok.

-spec get_use_funcs([tuple()], [binary()]) -> [binary()].
%% @doc get the name of the functions used in the code (no methods).
get_use_funcs([#call{type = normal, name = Name} = Tuple|Tuples], R)
        when is_binary(Name) ->
    get_use_funcs(erlang:tuple_to_list(Tuple), []) ++
    get_use_funcs(Tuples, [ephp_string:to_lower(Name)|R]);
get_use_funcs([{object,_,_}|Tuples], R) ->
    get_use_funcs(Tuples, R);
get_use_funcs([Tuple|Tuples], R) when is_tuple(Tuple) ->
    get_use_funcs(erlang:tuple_to_list(Tuple), []) ++
    get_use_funcs(Tuples, R);
get_use_funcs([List|Rest], R) when is_list(List) ->
    get_use_funcs(List, []) ++ get_use_funcs(Rest, R);
get_use_funcs([_Other|Rest], R) ->
    get_use_funcs(Rest, R);
get_use_funcs([], R) ->
    ordsets:from_list(R).


-spec get_defined_funcs([tuple()], [binary()]) -> [binary()].
%% @doc get the name of the functions used in the code (no methods).
get_defined_funcs([#function{name = Name} = Tuple|Tuples], R) ->
    get_defined_funcs(erlang:tuple_to_list(Tuple), []) ++
    get_defined_funcs(Tuples, [ephp_string:to_lower(Name)|R]);
get_defined_funcs([Tuple|Tuples], R) when is_tuple(Tuple) ->
    get_defined_funcs(erlang:tuple_to_list(Tuple), []) ++
    get_defined_funcs(Tuples, R);
get_defined_funcs([List|Rest], R) when is_list(List) ->
    get_defined_funcs(List, []) ++ get_defined_funcs(Rest, R);
get_defined_funcs([_Other|Rest], R) ->
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
             I + 1, J + 1}
        catch _:_ ->
            io:format(": FAIL~n", []),
            {Funcs, DefFuncs, I, J + 1}
        end
    end, {[], [], 0, 0}, Files),
    {Funcs -- DefFuncs, TotalOk, Total}.

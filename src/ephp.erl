-module(ephp).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    context_new/0,
    context_new/1,
    register_var/3,
    register_func/5,
    register_module/2,
    run/2,
    eval/2,
    eval/3,

    start/0,
    main/1   %% for escriptize
]).

-export_type([
    context_id/0,
    vars_id/0,
    output_id/0,
    funcs_id/0,
    classes_id/0,
    consts_id/0,
    includes_id/0,
    shutdown_id/0,
    errors_id/0,
    data_id/0
]).

-opaque context_id() :: reference().
-opaque vars_id() :: reference().
-opaque output_id() :: reference().
-opaque funcs_id() :: reference().
-opaque classes_id() :: reference().
-opaque consts_id() :: reference().
-opaque includes_id() :: reference().
-opaque shutdown_id() :: reference().
-opaque errors_id() :: reference().
-opaque data_id() :: reference().

-include("ephp.hrl").

-spec context_new() ->
    {ok, context()} | {error, Reason::term()}.

context_new() ->
    context_new(undefined).

-spec context_new(Filename :: binary()) ->
    {ok, context()} | {error, Reason::term()}.

context_new(Filename) ->
    Modules = ?MODULES,
    case ephp_context:start_link() of
        {ok, Ctx} ->
            [ register_module(Ctx, Module) || Module <- Modules ],
            ephp_context:set_active_file(Ctx, Filename),
            {ok, Ctx};
        Error ->
            Error
    end.

-type values() :: integer() | binary() | float() | ephp_array().

-spec register_var(Ctx :: context(), Var :: binary(), Value :: values()) ->
    ok | {error, reason()}.

register_var(Ctx, Var, Value) when
        is_reference(Ctx) andalso
        (is_integer(Value) orelse
        is_float(Value) orelse
        is_binary(Value) orelse
        ?IS_ARRAY(Value)) ->
    ephp_context:set(Ctx, #variable{name=Var}, Value);

register_var(_Ctx, _Var, _Value) ->
    {error, badarg}.

-spec register_func(
    Ctx :: context(), PHPName :: binary(),
    Module :: atom(), Fun :: atom(),
    PackArgs :: boolean()) -> ok | {error, reason()}.

register_func(Ctx, PHPName, Module, Fun, PackArgs) ->
    ephp_context:register_func(Ctx, PHPName, Module, Fun, PackArgs).

-spec register_module(Ctx :: context(), Module :: atom()) -> ok.

register_module(Ctx, Module) ->
    lists:foreach(fun
        ({Func, Opts}) ->
            PackArgs = proplists:get_value(pack_args, Opts, false),
            Name = proplists:get_value(alias, Opts, atom_to_binary(Func, utf8)),
            ephp:register_func(Ctx, Name, Module, Func, PackArgs);
        (Func) ->
            Name = atom_to_binary(Func, utf8),
            ephp:register_func(Ctx, Name, Module, Func, false)
    end, Module:init_func()).

-spec run(Context :: context(), Compiled :: [statement()]) ->
    {ok, binary()} | {error, Reason::reason()}.

run(Context, Compiled) ->
    ephp_interpr:process(Context, Compiled).

-spec eval(Context :: context(), PHP :: string() | binary()) ->
    {ok, Result :: binary()} | {error, Reason :: reason()} |
    {error,{Code::binary(), Line::integer(), Col::integer()}}.

eval(Context, PHP) ->
    eval(<<"-">>, Context, PHP).

-spec eval(Filename :: binary(), Context :: context(),
        PHP :: string() | binary()) ->
    {ok, Result :: binary()} | {error, Reason :: reason()} |
    {error,{Code::binary(), Line::integer(), Col::integer()}}.

eval(Filename, Context, PHP) ->
    case catch ephp_parser:parse(PHP) of
        {error, eparse, Line, _ErrorLevel, _Text} ->
            ephp_error:handle_error(Context, {error, eparse, Line,
                ?E_PARSE bor ?E_ERROR, Filename}),
            {error, eparse};
        Compiled ->
            case catch ephp_interpr:process(Context, Compiled) of
                {ok, Return} ->
                    {ok, Return};
                die ->
                    {ok, undefined};
                {error, Reason, _, _, _}=Error ->
                    ephp_error:handle_error(Context, Error),
                    {error, Reason}
            end
    end.

-spec start() -> ok.

start() ->
    application:start(ezic),
    application:start(zucchini),
    application:start(ephp),
    ok.

-spec main(Args :: [string()]) -> integer().

main([Filename]) ->
    start(),
    case file:read_file(Filename) of
    {ok, Content} ->
        start_profiling(),
        AbsFilename = list_to_binary(filename:absname(Filename)),
        ephp_config:start_link(?PHP_INI_FILE),
        {ok, Ctx} = context_new(AbsFilename),
        case eval(AbsFilename, Ctx, Content) of
            {ok, _Return} ->
                Result = ephp_context:get_output(Ctx),
                io:format("~s", [Result]),
                ephp_context:destroy_all(Ctx),
                stop_profiling(),
                0;
            {error, _Reason} ->
                stop_profiling(),
                -1
        end;
    {error, enoent} ->
        io:format("File not found: ~s~n", [Filename]),
        -2;
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason]),
        -3
    end;

main(_) ->
    io:format("Usage: ephp <file.php>~n", []),
    -1.


-ifdef(PROFILING).

start_profiling() ->
    eprof:start(),
    eprof:start_profiling([self()]).

stop_profiling() ->
    eprof:stop_profiling(),
    eprof:analyze(total).

-else.

start_profiling() ->
    ok.

stop_profiling() ->
    ok.

-endif.

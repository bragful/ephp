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
    errors_id/0
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

-include("ephp.hrl").

-spec context_new() -> {ok, context()}.

context_new() ->
    context_new(<<"-">>).

-spec context_new(Filename :: binary()) -> {ok, context()}.

context_new(Filename) ->
    Modules = application:get_env(ephp, modules, []),
    {ok, Ctx} = ephp_context:start_link(),
    [ register_module(Ctx, Module) || Module <- Modules ],
    ephp_context:set_active_file(Ctx, Filename),
    {ok, Ctx}.

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

-spec register_func(context(), PHPName :: binary(), module(), Fun :: atom(),
                    PackArgs :: boolean(),
                    Args :: ephp_func:validation_args()
                   ) -> ok | {error, reason()}.

register_func(Ctx, PHPName, Module, Fun, PackArgs, Args) ->
    ephp_context:register_func(Ctx, PHPName, Module, Fun, PackArgs, Args).

-spec register_module(context(), module()) -> ok.

register_module(Ctx, Module) ->
    ephp_config:module_init(Module),
    case proplists:get_value(handle_error, Module:module_info(exports)) of
        3 -> ephp_error:add_message_handler(Ctx, Module);
        _ -> ok
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

eval(Context, PHP) ->
    eval(<<"-">>, Context, PHP).

-spec eval(Filename :: binary(), context(), PHP :: string() | binary()) ->
    {ok, Result :: binary()} | {error, reason()} |
    {error, {Code::binary(), Line::integer(), Col::integer()}}.

eval(Filename, Context, PHP) ->
    case catch ephp_parser:parse(PHP) of
        {error, eparse, Line, _ErrorLevel, _Text} ->
            ephp_error:handle_error(Context, {error, eparse, Line,
                Filename, ?E_PARSE, {}}),
            {error, eparse};
        Compiled ->
            Cover = ephp_cover:get_config(),
            case catch ephp_interpr:process(Context, Compiled, Cover) of
                {ok, Return} ->
                    ephp_shutdown:shutdown(Context),
                    {ok, Return};
                {error, Reason, Index, Level, Data} ->
                    File = ephp_context:get_active_file(Context),
                    Error = {error, Reason, Index, File, Level, Data},
                    ephp_error:handle_error(Context, Error),
                    ephp_shutdown:shutdown(Context),
                    {error, Reason}
            end
    end.

-spec start() -> ok.

start() ->
    application:start(ezic),
    application:start(zucchini),
    application:start(ephp),
    application:set_env(ephp, modules, ?MODULES),
    ok.

-spec main(Args :: [string()]) -> integer().

main([Filename]) ->
    start(),
    case file:read_file(Filename) of
    {ok, Content} ->
        start_profiling(),
        start_cover(),
        AbsFilename = list_to_binary(filename:absname(Filename)),
        ephp_config:start_link(?PHP_INI_FILE),
        {ok, Ctx} = context_new(AbsFilename),
        case eval(AbsFilename, Ctx, Content) of
            {ok, _Return} ->
                Result = ephp_context:get_output(Ctx),
                io:format("~s", [Result]),
                ephp_context:destroy_all(Ctx),
                stop_profiling(),
                stop_cover(),
                quit(0);
            {error, _Reason} ->
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
quit(Code) ->
    erlang:halt(Code).
-else.
-spec quit(integer()) -> integer().
quit(Code) ->
    Code.
-endif.

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

start_cover() ->
    ephp_cover:start_link().

stop_cover() ->
    ephp_cover:dump().

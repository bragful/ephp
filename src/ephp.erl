-module(ephp).
-compile([warnings_as_errors]).

-export([
    context_new/0,
    context_new/1,
    context_new/2,
    register_var/3,
    register_func/5,
    register_module/2,
    run/2,
    eval/2,
    eval/3,

    main/1   %% for escriptize
]).

-include("ephp.hrl").

-spec context_new() -> 
    {ok, context()} | {error, Reason::term()}.

context_new() ->
    {ok, Cwd} = file:get_cwd(),
    context_new(<<"php shell code">>, list_to_binary(Cwd)).

-spec context_new(Filename :: binary()) -> 
    {ok, context()} | {error, Reason::term()}.

context_new(Filename) ->
    context_new(Filename, filename:dirname(Filename)).

-spec context_new(Filename :: binary(), Dirname :: binary()) -> 
    {ok, context()} | {error, Reason::term()}.

context_new(Filename, Dirname) ->
    Modules = ?MODULES,
    case ephp_context:start_link() of
        {ok, Ctx} -> 
            [ register_module(Ctx, Module) || Module <- Modules ],
            ephp_context:register_const(Ctx, <<"__FILE__">>, Filename),
            ephp_context:register_const(Ctx, <<"__DIR__">>, Dirname),
            ephp_context:register_const(Ctx, <<"__FUNCTION__">>, <<>>), 
            {ok, Ctx};
        Error ->
            Error
    end.

-type values() :: integer() | binary() | float() | ?DICT_TYPE.

-spec register_var(Ctx :: context(), Var :: binary(), Value :: values()) ->
    ok | {error, reason()}.

register_var(Ctx, Var, Value) when
        is_reference(Ctx) andalso 
        (is_integer(Value) orelse 
        is_float(Value) orelse
        is_binary(Value) orelse
        ?IS_DICT(Value)) ->
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
        ({Func, Name, PackArgs}) ->
            ephp:register_func(Ctx, Name, Module, Func, PackArgs);
        ({Func, PackArgs}) when is_boolean(PackArgs) ->
            Name = atom_to_binary(Func, utf8),
            ephp:register_func(Ctx, Name, Module, Func, PackArgs);
        ({Func, Name}) ->
            ephp:register_func(Ctx, Name, Module, Func, false);
        (Func) ->
            Name = atom_to_binary(Func, utf8),
            ephp:register_func(Ctx, Name, Module, Func, false)
    end, Module:init()).

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
        {error, eparse, Line, _Text} ->
            ErrorText = io_lib:format("~nParse Error: parse error in ~s "
                "on line ~p~n",
                [Filename, ephp_util:get_line(Line)]),
            ephp_context:set_output(Context, ErrorText), 
            {ok, null};
        Compiled ->
            ephp_interpr:process(Context, Compiled)
    end.

-spec main(Args :: [string()]) -> integer().

main([Filename]) ->
    case file:read_file(Filename) of
    {ok, Content} ->
        start_profiling(),
        AbsFilename = list_to_binary(filename:absname(Filename)),
        {ok, Ctx} = context_new(AbsFilename),
        {ok, _Return} = eval(AbsFilename, Ctx, Content),
        Result = ephp_context:get_output(Ctx),
        io:format("~s", [Result]),
        stop_profiling(),
        0;
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

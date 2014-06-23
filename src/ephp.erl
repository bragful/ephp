-module(ephp).
-compile([warnings_as_errors]).

-export([
    context_new/0,
    context_new/1,
    context_new/2,
    register_var/3,
    register_fun/4,
    register_module/2,
    compile/1,
    run/2,
    eval/2,

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

-type values() :: integer() | binary() | float() | dict().

-spec register_var(Ctx :: context(), Var :: binary(), Value :: values()) ->
    ok | {error, reason()}.

register_var(Ctx, _Var, _Value) when not is_pid(Ctx) ->
    {error, enoctx};

register_var(Ctx, Var, Value) when 
        is_integer(Value) orelse 
        is_float(Value) orelse
        is_binary(Value) orelse
        ?IS_DICT(Value) ->
    ephp_context:set(Ctx, #variable{name=Var}, Value);

register_var(_Ctx, _Var, _Value) ->
    {error, badarg}.

-spec register_fun(
    Ctx :: context(), PHPName :: binary(), 
    Module :: atom(), Fun :: atom()) -> ok | {error, reason()}.

register_fun(Ctx, _PHPName, _Module, _Fun) when not is_pid(Ctx) ->
    {error, enoctx};

register_fun(Ctx, PHPName, Module, Fun) ->
    ephp_context:register_func(Ctx, PHPName, Module, Fun).

-spec register_module(Ctx :: context(), Module :: atom()) -> ok.

register_module(Ctx, Module) ->
    Module:init(Ctx).

-spec compile(PHP :: binary() | string()) -> 
    {ok, Result :: [statement()]} | {error, Reason :: reason()} | 
    {error,{Code::binary(), Line::integer(), Col::integer()}}.

compile(PHP) ->
    case ephp_parser:parse(PHP) of
        {_,Code,{{line,Line},{column,Col}}} ->
            {error, {Code,Line,Col}};
        Compiled ->
            {ok, Compiled}
    end.

-spec run(Context :: context(), Compiled :: [statement()]) -> 
    {ok, binary()} | {error, Reason::reason()}.

run(Context, Compiled) ->
    ephp_interpr:process(Context, Compiled).

-spec eval(Context :: context(), PHP :: string() | binary()) -> 
    {ok, Result :: binary()} | {error, Reason :: reason()} | 
    {error,{Code::binary(), Line::integer(), Col::integer()}}.

eval(Context, PHP) ->
    case ephp_parser:parse(PHP) of
        {_,Code,{{line,Line},{column,Col}}} ->
            {error, {Code,Line,Col}};
        Compiled ->
            ephp_interpr:process(Context, Compiled)
    end.

-spec main(Args :: [string()]) -> integer().

main([Filename]) ->
    case file:read_file(Filename) of
    {ok, Content} ->
        % eprof:start(),
        % eprof:start_profiling([self()]),
        {ok, Ctx} = context_new(list_to_binary(filename:absname(Filename))),
        {ok, Result} = eval(Ctx, Content),
        io:format("~s", [Result]),
        % eprof:stop_profiling(),
        % eprof:analyze(total),
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

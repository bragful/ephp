-module(ephp).
-compile([warnings_as_errors]).

-export([
    context_new/0,
    register_var/3,
    register_fun/4,
    eval/2
]).

-include("ephp.hrl").

-spec context_new() -> {ok, context()}.

context_new() ->
    ephp_context:start_link().

-type values() :: integer() | binary() | float().

-spec register_var(Ctx :: context(), Var :: binary(), Value :: values()) ->
    ok | {error, reason()}.

register_var(Ctx, _Var, _Value) when not is_pid(Ctx) ->
    {error, enoctx};

register_var(Ctx, Var, Value) when 
        is_integer(Value) orelse 
        is_float(Value) orelse
        is_binary(Value) ->
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

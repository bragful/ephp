-module(ephp).

-include("ephp.hrl").

-spec context_new() -> context().

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

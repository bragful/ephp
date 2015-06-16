-module(ephp_func).

-compile([warnings_as_errors]).

-include("ephp.hrl").

-type php_function() :: atom().
-type php_function_alias() :: binary().

-export_type([
    php_function/0,
    php_function_alias/0
]).

-callback init() -> [php_function()].

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    destroy/1,

    get/2,
    get_functions/1,

    run/2,
    call/3,

    register_func/3,
    register_func/4,
    register_func/5
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, ?DICT:new()),
    {ok, Ref}.

destroy(Funcs) ->
    erlang:erase(Funcs).

get(Ref, FuncName) ->
    Funcs = erlang:get(Ref),
    IFuncName = ephp_util:to_lower(FuncName),
    ?DICT:find(IFuncName, Funcs).

get_functions(Ref) ->
    Funcs = erlang:get(Ref),
    Get = fun
        (#reg_func{name=Name, type=builtin}) -> {Name, <<"internal">>};
        (#reg_func{name=Name, type=php}) -> {Name, <<"user">>}
    end,
    [ Get(FuncName) || {_,FuncName} <- ?DICT:to_list(Funcs) ].

register_func(Ref, PHPFunc, Module, Fun)
        when is_atom(Module) andalso is_atom(Fun) ->
    register_func(Ref, PHPFunc, Module, Fun, false);

register_func(Ref, PHPFunc, Fun, PackArgs) when is_function(Fun) -> 
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_util:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=builtin,
        builtin=Fun,
        pack_args=PackArgs},
    erlang:put(Ref, ?DICT:store(IPHPFunc, RegFunc, Funcs)),
    ok;

register_func(Ref, PHPFunc, Args, Code) ->
    register_func(Ref, PHPFunc, Args, Code, false).

register_func(Ref, PHPFunc, Fun) ->
    register_func(Ref, PHPFunc, Fun, false).

register_func(Ref, PHPFunc, Module, Fun, PackArgs)
        when is_atom(Module) andalso is_atom(Fun) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_util:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=builtin,
        builtin={Module, Fun},
        pack_args=PackArgs},
    erlang:put(Ref, ?DICT:store(IPHPFunc, RegFunc, Funcs)),
    ok;

register_func(Ref, PHPFunc, Args, Code, PackArgs) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_util:to_lower(PHPFunc),
    RegFunc = #reg_func{
        name=IPHPFunc,
        type=php,
        args=Args,
        code=Code,
        pack_args=PackArgs},
    erlang:put(Ref, ?DICT:store(IPHPFunc, RegFunc, Funcs)),
    ok.


run(Context, #call{line=Line}=Call) ->
    try 
        ephp_context:solve(Context, Call),
        false
    catch
        throw:die ->
            {return, null};
        throw:{error, erequired, Ln, ReqFile} ->
            File = ephp_context:get_const(Context, <<"__FILE__">>),
            Error = io_lib:format(
                "~nFatal error: require(): Failed opening required '~s'"
                " in ~s on line ~p~n",
                [ReqFile, File, Ln]),
            ephp_context:set_output(Context, Error),
            {return, null}; 
        throw:{error, eundefun, _, Fun} ->
            %% TODO: format better the output errors
            File = ephp_context:get_const(Context, <<"__FILE__">>),
            Error = io_lib:format(
                "~nFatal error: Call to undefined function ~s()"
                " in ~s on line ~p~n",
                [Fun, File, ephp_util:get_line(Line)]),
            ephp_context:set_output(Context, Error),
            {return, null}
    end.

call(Funcs, PHPFunc, Args) ->
    case get(Funcs, PHPFunc) of
    error ->
        throw(eundefun);
    {ok, #reg_func{type=builtin, pack_args=false, builtin={Module, Fun}}} ->
        fun(Ctx) ->
            erlang:apply(Module, Fun, [Ctx|Args])
        end;
    {ok, #reg_func{type=builtin, pack_args=true, builtin={Module, Fun}}} ->
        fun(Ctx) ->
            erlang:apply(Module, Fun, [Ctx|[Args]])
        end;
    {ok, #reg_func{type=builtin, pack_args=false, builtin=Fun}} ->
        fun(Ctx) ->
            erlang:apply(Fun, [Ctx|Args])
        end;
    {ok, #reg_func{type=builtin, pack_args=true, builtin=Fun}} ->
        fun(Ctx) ->
            erlang:apply(Fun, [Ctx|[Args]])
        end;
    {ok, #reg_func{type=php, code=Code}} ->
        fun(Ctx) ->
            OldFunc = ephp_context:get_const(Ctx, <<"__FUNCTION__">>),
            OldNArgs = ephp_context:get_const(Ctx, <<"__FUNCT_NUM_ARGS__">>),
            ephp_context:register_const(Ctx, <<"__FUNCTION__">>, PHPFunc),
            ephp_context:register_const(Ctx, <<"__FUNCT_NUM_ARGS__">>,
                length(Args)),
            Res = ephp_interpr:process(Ctx, Code),
            ephp_context:register_const(Ctx, <<"__FUNCTION__">>, OldFunc),
            ephp_context:register_const(Ctx, <<"__FUNCT_NUM_ARGS__">>,
                OldNArgs),
            Res
        end
    end.

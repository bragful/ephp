-module(ephp_func).

-compile([warnings_as_errors]).

-include("ephp.hrl").

-record(reg_func, {
    name :: binary(),
    args :: [variable()],
    type :: builtin | php,
    code = [] :: [statement()],
    builtin :: {Module :: atom(), Func :: atom()} | function()
}).

-type php_function() :: atom().

-callback init() -> [php_function()].

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    destroy/1,

    get/2,

    run/2,
    call/3,

    register_func/3,
    register_func/4
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

register_func(Ref, PHPFunc, Module, Fun) when is_atom(Module) and is_atom(Fun) ->  
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_util:to_lower(PHPFunc),
    RegFunc = #reg_func{name=IPHPFunc, type=builtin, builtin={Module, Fun}},
    erlang:put(Ref, ?DICT:store(IPHPFunc, RegFunc, Funcs)),
    ok;

register_func(Ref, PHPFunc, Args, Code) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_util:to_lower(PHPFunc),
    RegFunc = #reg_func{name=IPHPFunc, type=php, args=Args, code=Code},
    erlang:put(Ref, ?DICT:store(IPHPFunc, RegFunc, Funcs)),
    ok.

register_func(Ref, PHPFunc, Fun) ->
    Funcs = erlang:get(Ref),
    IPHPFunc = ephp_util:to_lower(PHPFunc),
    RegFunc = #reg_func{name=IPHPFunc, type=builtin, builtin=Fun},
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
        %% TODO: enhance the error handling!
        io:format("~p~n", [{PHPFunc, Args}]),
        throw(eundefun);
    {ok, #reg_func{type=builtin, builtin={Module, Fun}}} ->
        fun(Ctx) -> 
            erlang:apply(Module, Fun, [Ctx|Args]) 
        end;
    {ok, #reg_func{type=builtin, builtin=Fun}} ->
        fun(Ctx) -> 
            erlang:apply(Fun, [Ctx|Args]) 
        end;
    {ok, #reg_func{type=php, code=Code}} ->
        fun(Ctx) ->
            OldFunc = ephp_context:get_const(Ctx, <<"__FUNCTION__">>),
            ephp_context:register_const(Ctx, <<"__FUNCTION__">>, PHPFunc), 
            Res = ephp_interpr:process(Ctx, Code),
            ephp_context:register_const(Ctx, <<"__FUNCTION__">>, OldFunc),
            Res
        end
    end.

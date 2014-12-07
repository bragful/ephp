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

-callback init(Context :: context()) -> ok.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    destroy/1,

    get/2,

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
    ?DICT:find(FuncName, Funcs).

register_func(Ref, PHPFunc, Module, Fun) when is_atom(Module) and is_atom(Fun) ->  
    Funcs = erlang:get(Ref),
    RegFunc = #reg_func{name=PHPFunc, type=builtin, builtin={Module, Fun}},
    erlang:put(Ref, ?DICT:store(PHPFunc, RegFunc, Funcs)),
    ok;

register_func(Ref, PHPFunc, Args, Code) ->
    Funcs = erlang:get(Ref),
    RegFunc = #reg_func{name=PHPFunc, type=php, args=Args, code=Code},
    erlang:put(Ref, ?DICT:store(PHPFunc, RegFunc, Funcs)),
    ok.

register_func(Ref, PHPFunc, Fun) ->
    Funcs = erlang:get(Ref),
    RegFunc = #reg_func{name=PHPFunc, type=builtin, builtin=Fun},
    erlang:put(Ref, ?DICT:store(PHPFunc, RegFunc, Funcs)),
    ok.

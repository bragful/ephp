-module(ephp_func).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile([warnings_as_errors]).

-include("ephp.hrl").

-record(reg_func, {
    name :: binary(),
    args :: [variable()],
    type :: builtin | php,
    code = [] :: [statement()],
    builtin :: {Module :: atom(), Func :: atom()} | function()
}).

-record(state, {
    funcs = ?DICT:new() :: dict()
}).

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
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

destroy(Funcs) ->
    gen_server:cast(Funcs, stop).

get(Funcs, FuncName) ->
    gen_server:call(Funcs, {get, FuncName}).

register_func(Funcs, PHPFunc, Module, Fun) when is_atom(Module) and is_atom(Fun) ->  
    gen_server:cast(Funcs, {register, builtin, PHPFunc, Module, Fun});

register_func(Funcs, PHPFunc, Args, Code) ->
    gen_server:cast(Funcs, {register, php, PHPFunc, Args, Code}).

register_func(Funcs, PHPFunc, Fun) ->
    gen_server:cast(Funcs, {register, builtin, PHPFunc, Fun}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({get, FuncName}, _From, #state{funcs=Funcs}=State) ->
    {reply, ?DICT:find(FuncName, Funcs), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({register, php, PHPFunc, Args, Code}, #state{funcs=Funcs}=State) ->
    RegFunc = #reg_func{name=PHPFunc, type=php, args=Args, code=Code},
    {noreply, State#state{funcs=?DICT:store(PHPFunc, RegFunc, Funcs)}};

handle_cast({register, builtin, PHPFunc, Fun}, #state{funcs=Funcs}=State) ->
    RegFunc = #reg_func{name=PHPFunc, type=builtin, builtin=Fun},
    {noreply, State#state{funcs=?DICT:store(PHPFunc, RegFunc, Funcs)}};

handle_cast({register, builtin, PHPFunc, Module, Fun}, #state{funcs=Funcs}=State) ->
    RegFunc = #reg_func{name=PHPFunc, type=builtin, builtin={Module, Fun}},
    {noreply, State#state{funcs=?DICT:store(PHPFunc, RegFunc, Funcs)}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


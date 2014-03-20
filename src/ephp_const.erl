-module(ephp_const).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile([warnings_as_errors]).

-include("ephp.hrl").

-record(state, {
    const = ?DICT:new() :: dict()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    get/2,
    set/3,
    destroy/1
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

get(Const, Name) ->
    gen_server:call(Const, {get, Name}).

set(Const, Name, Value) ->
    gen_server:cast(Const, {set, Name, Value}).

destroy(Const) ->
    gen_server:cast(Const, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({get, Name}, _From, #state{const=Const}=State) ->
    {reply, case ?DICT:find(Name, Const) of
        {ok, Value} -> Value;
        error -> Name
    end, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set, Name, Value}, #state{const=Const}=State) ->
    NewConst = ?DICT:store(Name, Value, Const),
    {noreply, State#state{const=NewConst}};

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

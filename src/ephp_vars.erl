-module(ephp_vars).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile([warnings_as_errors]).

-include("ephp.hrl").

-record(state, {
    vars = ?DICT:new() :: dict()
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

get(Context, VarPath) ->
    gen_server:call(Context, {get, VarPath}).

set(Context, VarPath, Value) when is_record(Value, value) ->
    gen_server:cast(Context, {set, VarPath, Value});

set(Context, VarPath, Value) ->
    ValueFormatted = #value{content=Value, type=value},
    gen_server:cast(Context, {set, VarPath, ValueFormatted}).

destroy(Context) ->
    gen_server:cast(Context, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({get, VarPath}, _From, State) ->
    Value = search(VarPath, State#state.vars),
    {reply, Value, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({set, VarPath, Value}, State) ->
    NewVars = change(VarPath, Value, State#state.vars),
    {noreply, State#state{vars = NewVars}};

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

search(#variable{name=Root, idx=[]}, Vars) ->
    case ?DICT:find(Root, Vars) of
        error -> undefined;
        {ok, Value} -> Value
    end;

search(#variable{name=Root, idx=[NewRoot|Idx]}, Vars) ->
    case ?DICT:find(Root, Vars) of
        {ok, NewVars} when ?IS_DICT(NewVars) -> 
            search(#variable{name=NewRoot, idx=Idx}, NewVars);
        _ -> 
            undefined
    end.


change(#variable{name=Root, idx=[]}, #value{content=undefined}, Vars) ->
    ?DICT:erase(Root, Vars);

change(#variable{name=Root, idx=[]}, #value{}=Value, Vars) ->
    ?DICT:store(Root, Value, Vars);

change(#variable{name=Root, idx=[NewRoot|Idx]}, #value{}=Value, Vars) ->
    case ?DICT:find(Root, Vars) of
    {ok, NewVars} when ?IS_DICT(NewVars) -> 
        ?DICT:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value, NewVars), Vars);
    _ -> 
        ?DICT:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value, ?DICT:new()), Vars)
    end.

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
    ref/4,
    del/2,
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

set(Context, VarPath, Value) ->
    ValueFormatted = Value,
    gen_server:cast(Context, {set, VarPath, ValueFormatted}).

ref(Context, VarPath, VarsPID, RefVarPath) ->
    ValueFormatted = #var_ref{pid=VarsPID, ref=RefVarPath},
    gen_server:cast(Context, {set, VarPath, ValueFormatted}).

del(Context, VarPath) ->
    gen_server:cast(Context, {set, VarPath, undefined}).

destroy(Context) ->
    gen_server:cast(Context, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({get, VarPath}, From, State) ->
    case search(VarPath, From, State#state.vars) of
        {ok, Value} -> {reply, Value, State};
        noreply -> {noreply, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({set, VarPath, Value}, State) ->
    NewVars = change(VarPath, Value, State#state.vars),
    {noreply, State#state{vars = NewVars}};

handle_cast({get, From, VarPath}, State) ->
    case search(VarPath, From, State#state.vars) of
        {ok, Value} -> gen_server:reply(From, Value);
        noreply -> ok
    end,
    {noreply, State};

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

search(#variable{name = <<"GLOBALS">>, idx=[]}, _From, Vars) ->
    {ok, Vars};

search(#variable{name = <<"GLOBALS">>, idx=[Root|Idx]}, From, Vars) ->
    search(#variable{name=Root, idx=Idx}, From, Vars);

search(#variable{name=Root, idx=[]}, From, Vars) ->
    case ?DICT:find(Root, Vars) of
        error ->
            {ok, undefined};
        {ok, #var_ref{pid=RefVarsPID, ref=RefVar}} ->
            gen_server:cast(RefVarsPID, {get, From, RefVar}),
            noreply;
        {ok, Value} ->
            {ok, Value}
    end;

search(#variable{name=Root, idx=[NewRoot|Idx]}, From, Vars) ->
    case ?DICT:find(Root, Vars) of
        {ok, #var_ref{pid=RefVarsPID, ref=#variable{idx=NewIdx}=RefVar}} ->
            NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
            gen_server:cast(RefVarsPID, {get, From, NewRefVar}),
            noreply;
        {ok, NewVars} -> 
            search(#variable{name=NewRoot, idx=Idx}, From, NewVars);
        _ -> 
            {ok, undefined}
    end.

change(#variable{name = <<"GLOBALS">>, idx=[]}, Value, _Vars) when ?IS_DICT(Value) ->
    lists:foldl(fun({Root,Val}, NewVars) ->
        ?DICT:store(Root, Val, NewVars)
    end, ?DICT:new(), ?DICT:to_list(Value));

change(#variable{name = <<"GLOBALS">>, idx=[Root|Idx]}, Value, Vars) ->
    change(#variable{name=Root, idx=Idx}, Value, Vars);

change(#variable{name=Root, idx=[]}=_Var, undefined, Vars) ->
    ?DICT:erase(Root, Vars);

change(#variable{name=Root, idx=[]}=_Var, Value, Vars) ->
    case ?DICT:find(Root, Vars) of
    {ok, #var_ref{pid=RefVarsPID, ref=RefVar}} ->
        gen_server:cast(RefVarsPID, {set, RefVar, Value}),
        Vars;
    _ ->
        ?DICT:store(Root, Value, Vars)
    end;

change(#variable{name=Root, idx=[NewRoot|Idx]}=_Var, Value, Vars) ->
    case ?DICT:find(Root, Vars) of
    {ok, #var_ref{pid=RefVarsPID, ref=#variable{idx=NewIdx}=RefVar}} ->
        NewRefVar = RefVar#variable{idx = NewIdx ++ [NewRoot|Idx]},
        gen_server:cast(RefVarsPID, {set, NewRefVar, Value}),
        Vars;
    {ok, NewVars} when ?IS_DICT(NewVars) -> 
        ?DICT:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value, NewVars), Vars);
    _ -> 
        ?DICT:store(Root, change(#variable{name=NewRoot, idx=Idx}, Value, ?DICT:new()), Vars)
    end.

-module(ephp_include).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-compile([warnings_as_errors]).

-include("ephp.hrl").

-record(state, {
    include = ?DICT:new() :: dict()
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    load_once/2,
    load/2,
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

load_once(Inc, Name) ->
    gen_server:call(Inc, {load_once, Name}).

load(Inc, Name) ->
    gen_server:call(Inc, {load, Name}).

destroy(Inc) ->
    gen_server:cast(Inc, stop).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({load, Name}, _From, #state{include=Inc}=State) ->
    case ?DICT:find(Name, Inc) of
    {ok, Value} ->
        {reply, Value, State};
    error ->
        try
            {ok, Content} = file:read_file(Name),
            Value = ephp_parser:parse(Content),
            NewInc = ?DICT:store(Name, Value, Inc), 
            {reply, Value, State#state{include=NewInc}}
        catch
            _:Reason -> {reply, {error, Reason}, State}
        end
    end;

handle_call({load_once, Name}, _From, #state{include=Inc}=State) ->
    case ?DICT:find(Name, Inc) of
    {ok, _Value} ->
        {reply, {error, duplicated}, State};
    error ->
        try
            {ok, Content} = file:read_file(Name),
            Value = ephp_parser:parse(Content),
            NewInc = ?DICT:store(Name, Value, Inc), 
            {reply, Value, State#state{include=NewInc}}
        catch
            _:Reason -> {reply, {error, Reason}, State}
        end
    end;

handle_call(_Request, _From, State) ->
    io:format("unknown: ~p~n", [_Request]),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    io:format("unknown: ~p~n", [_Msg]),
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

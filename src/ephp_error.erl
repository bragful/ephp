-module(ephp_error).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    start_link/0,
    destroy/1,
    init_consts/0,

    set_output/2,

    run_quiet/2,

    error/1,
    handle_error/2
]).

-callback set_output(context(), string()) -> ok.

-include("ephp.hrl").

-type error_type() ::
    eundefclass |
    eprivateaccess |
    ebadbnot |
    erequired |
    einclude |
    enofile |
    eundefun |
    earrayundef |
    eundeftoken |
    enoclassscope |
    emethodtypes |
    eundefmethod |
    edivzero |
    eparse |
    enostatement |
    eunknownst |
    eassignthis.

-record(state, {
    handler = ?MODULE :: module(),
    silent = false :: boolean()
}).

-spec start_link() -> {ok, ephp:errors_id()}.

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, #state{}),
    {ok, Ref}.

destroy(Funcs) ->
    erlang:erase(Funcs).

init_consts() -> [
    {<<"E_ERROR">>, 1},
    {<<"E_WARNING">>, 2},
    {<<"E_PARSE">>, 4},
    {<<"E_NOTICE">>, 8},
    {<<"E_CORE_ERROR">>, 16},
    {<<"E_CORE_WARNING">>, 32},
    {<<"E_COMPILE_ERROR">>, 64},
    {<<"E_COMPILE_WARNING">>, 128},
    {<<"E_USER_ERROR">>, 256},
    {<<"E_USER_WARNING">>, 512},
    {<<"E_USER_NOTICE">>, 1024},
    {<<"E_STRICT">>, 2048},
    {<<"E_RECOVERABLE_ERROR">>, 4096},
    {<<"E_DEPRECATED">>, 8192},
    {<<"E_USER_DEPRECATED">>, 16384},
    {<<"E_ALL">>, 32767}
].

-type throw_error() ::
    atom() |
    {error, error_type(), line(), binary()}.

-spec error(throw_error()) -> ok.

error({error, Type, Index, Data}) ->
    throw({error, Type, Index, Data}).

-spec handle_error(context(), {error, error_type(), line(), binary()}) -> ok.

handle_error(Context, {error, Type, Index, Data}) ->
    Line = ephp_util:get_line(Index),
    ErrorText = get_message(Type, Line, Data),
    case erlang:get(ephp_context:get_errors_id(Context)) of
        #state{silent=true} ->
            ok;
        #state{handler=Module} ->
            Module:set_output(Context, iolist_to_binary(ErrorText))
    end,
    get_return(Type).

-spec set_output(context(), binary()) -> ok.

set_output(Context, Text) ->
    ephp_context:set_output(Context, Text).

-spec run_quiet(ephp:errors_id(), function()) -> ok.

run_quiet(Errors, Fun) ->
    case erlang:get(Errors) of
        #state{silent=true} ->
            Fun();
        State ->
            erlang:put(Errors, State#state{silent=true}),
            Result = Fun(),
            erlang:put(Errors, State#state{silent=false}),
            Result
    end.

-spec get_message(error_type(), pos_integer() | undefined, binary()) -> string().

get_message(eparse, Line, Filename) ->
    io_lib:format(
        "~nParse error: parse error in ~s on line ~p~n",
        [Filename, Line]);

get_message(erequired, Line, {File, ReqFile}) ->
    io_lib:format(
        "~nFatal error: require(): Failed opening required '~s'"
        " in ~s on line ~p~n",
        [ReqFile, File, Line]);

get_message(einclude, Line, {File, ReqFile, Func}) ->
    IncludePath = ephp_config:get(<<"include_path">>, <<".:">>),
    io_lib:format(
        "~nWarning: ~s(): Failed opening '~s' for inclusion (include_path='~s')"
        " in ~s on line ~p~n",
        [Func, ReqFile, IncludePath, File, Line]);

get_message(enofile, Line, {File, OpenFile, Func}) ->
    io_lib:format(
        "~nWarning: ~s(~s): failed to open stream: No such file or directory "
        "in ~s on line ~p~n",
        [Func, OpenFile, File, Line]);

get_message(eundefun, Line, {File, Fun}) ->
    io_lib:format(
        "~nFatal error: Call to undefined function ~s() in ~s on line ~p~n",
        [Fun, File, Line]);

get_message(eundefclass, Line, {File, <<>>}) ->
    io_lib:format(
        "~nFatal error: Cannot access self:: when no class scope is active "
        "in ~s on line ~p~n",
        [File, Line]);

get_message(eundefclass, Line, {File, Class}) ->
    io_lib:format(
        "~nFatal error: Class '~s' not found in ~s on line ~p~n",
        [Class, File, Line]);

get_message(eprivateaccess, Line, {File, Class, Element, Access}) ->
    io_lib:format(
        "~nFatal error: Cannot access ~s property ~s::~s in ~s on line ~p~n",
        [Access, Class, Element, File, Line]);

get_message(ebadbnot, Line, File) ->
    io_lib:format(
        "~nFatal error: Unsupported operand types in ~s on line ~p~n",
        [File, Line]);

get_message(eassignthis, Line, File) ->
    io_lib:format(
        "~nFatal error: Cannot re-assign $this in ~s on line ~p~n",
        [File, Line]);

get_message(ewrongarg, Line, {Function, ArgNum, ArgType, WrongType, File}) ->
    io_lib:format(
        "~nWarning: ~s() expects parameter ~p to be ~s, ~s given in ~s on line ~p~n",
        [Function, ArgNum, ArgType, WrongType, File, Line]);

get_message(Unknown, Line, Data) ->
    io_lib:format(
        "~nFatal Error: unknown ~p for ~p in line ~p~n",
        [Unknown, Data, Line]).

-spec get_return(error_type()) -> term().

get_return(eparse) -> {ok, null};
get_return(_) -> {return, null}.

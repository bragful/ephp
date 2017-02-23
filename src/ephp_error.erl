-module(ephp_error).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    start_link/0,
    destroy/1,

    set_output/2,
    set_output_handler/2,

    run_quiet/2,

    error_reporting/2,
    error/1,
    handle_error/2,
    get_line/1
]).

-callback set_output(context(), string()) -> ok.

-include("ephp.hrl").

-type error_type() ::
    eundefclass |
    eprivateaccess |
    ecallprivate |
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
    eundefvar |
    eundefconst |
    eassignthis |
    earrayconv.

-record(state, {
    handler = ?MODULE :: module(),
    silent = false :: boolean(),
    level = ?E_ALL
}).

-spec start_link() -> {ok, ephp:errors_id()}.

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, #state{}),
    {ok, Ref}.

destroy(Funcs) ->
    erlang:erase(Funcs).

-spec error_reporting(context(), integer()) -> integer().

error_reporting(Context, Level) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    erlang:put(ErrorsId, State#state{level=Level}),
    State#state.level.

-type throw_error() ::
    atom() |
    {error, error_type(), line(), error_level(), any()}.

-spec error(throw_error()) -> ok.

error({error, Type, Index, Level, Data}) ->
    throw({error, Type, Index, Level, Data}).

-spec handle_error(context(), {error, error_type(), line(),
    error_level(), any()}) -> ok.

handle_error(Context, {error, Type, Index, Level, Data}) ->
    Line = get_line(Index),
    ErrorText = get_message(Type, Line, get_level(Level), Data),
    case erlang:get(ephp_context:get_errors_id(Context)) of
        #state{silent=true} ->
            ok;
        #state{level=CfgLevel} when (CfgLevel band Level) =:= 0 ->
            ok;
        #state{handler=Module} ->
            Module:set_output(Context, iolist_to_binary(ErrorText))
    end,
    get_return(Type).

-spec set_output(context(), binary()) -> ok.

set_output(Context, Text) ->
    ephp_context:set_output(Context, Text).

-spec set_output_handler(context(), module()) -> ok.

set_output_handler(Context, Module) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    erlang:put(ErrorsId, State#state{handler=Module}),
    ok.

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

-spec get_message(error_type(), pos_integer() | undefined,
                  binary(), binary() | term()) -> string().

get_message(eparse, Line, _Level, Filename) ->
    io_lib:format(
        "~nParse error: parse error in ~s on line ~p~n",
        [Filename, Line]);

get_message(erequired, Line, _Level, {File, ReqFile, Func}) ->
    IncludePath = ephp_config:get(<<"include_path">>, <<".:">>),
    io_lib:format(
        "~nFatal error: ~s(): Failed opening required '~s'"
        " (include_path='~s') in ~s on line ~p~n",
        [Func, ReqFile, IncludePath, File, Line]);

get_message(einclude, Line, _Level, {File, ReqFile, Func}) ->
    IncludePath = ephp_config:get(<<"include_path">>, <<".:">>),
    io_lib:format(
        "~nWarning: ~s(): Failed opening '~s' for inclusion (include_path='~s')"
        " in ~s on line ~p~n",
        [Func, ReqFile, IncludePath, File, Line]);

get_message(enofile, Line, _Level, {File, OpenFile, Func}) ->
    io_lib:format(
        "~nWarning: ~s(~s): failed to open stream: No such file or directory "
        "in ~s on line ~p~n",
        [Func, OpenFile, File, Line]);

get_message(eundefun, Line, _Level, {File, Fun}) ->
    io_lib:format(
        "~nFatal error: Call to undefined function ~s() in ~s on line ~p~n",
        [Fun, File, Line]);

get_message(eundefclass, Line, _Level, {File, <<>>}) ->
    io_lib:format(
        "~nFatal error: Cannot access self:: when no class scope is active "
        "in ~s on line ~p~n",
        [File, Line]);

get_message(eundefclass, Line, Level, {File, Class}) ->
    io_lib:format(
        "~n~s: Class '~s' not found in ~s on line ~p~n",
        [Level, Class, File, Line]);

get_message(eprivateaccess, Line, _Level, {File, Class, Element, Access}) ->
    io_lib:format(
        "~nFatal error: Cannot access ~s property ~s::$~s in ~s on line ~p~n",
        [Access, Class, Element, File, Line]);

get_message(ecallprivate, Line, _Level, {File, Class, Element, Access}) ->
    io_lib:format(
        "~nFatal error: Call to ~s method ~s::~s() in ~s on line ~p~n",
        [Access, Class, Element, File, Line]);

get_message(ebadbnot, Line, _Level, File) ->
    io_lib:format(
        "~nFatal error: Unsupported operand types in ~s on line ~p~n",
        [File, Line]);

get_message(eassignthis, Line, _Level, File) ->
    io_lib:format(
        "~nFatal error: Cannot re-assign $this in ~s on line ~p~n",
        [File, Line]);

get_message(ewrongarg, Line, _Level, {Function, ArgNum, ArgType, WrongType, File}) ->
    io_lib:format(
        "~nWarning: ~s() expects parameter ~p to be ~s, ~s given in ~s on line ~p~n",
        [Function, ArgNum, ArgType, WrongType, File, Line]);

get_message(enoarray, Line, _Level, File) ->
    io_lib:format(
        "~nWarning:  Cannot use a scalar value as an array in ~s "
        "on line ~p~n",
        [File, Line]);

get_message(eundefvar, Line, _Level, {File, Var}) ->
    io_lib:format(
        "~nNotice: Undefined variable: ~s in ~s on line ~p~n",
        [Var, File, Line]);

get_message(eundefconst, Line, _Level, {File, Const}) ->
    io_lib:format(
        "~nNotice: Use of undefined constant ~s - assumed '~s' in ~s"
        " on line ~p~n",
        [Const, Const, File, Line]);

get_message(enotostring, Line, _Level, {File, ClassName}) ->
    io_lib:format(
        "~nCatchable fatal error: Object of class ~s could not be converted "
        "to string in ~s on line ~p~n",
        [ClassName, File, Line]);

get_message(enocast, Line, _Level, {File, ClassName, Type}) ->
    io_lib:format(
        "~nNotice: Object of class ~s could not be converted "
        "to ~s in ~s on line ~p~n",
        [ClassName, Type, File, Line]);

get_message(earrayconv, Line, _Level, {File, Type}) ->
    io_lib:format(
        "~nNotice: Array to ~s conversion in ~s on line ~p~n",
        [Type, File, Line]);

get_message(Unknown, Line, _Level, Data) ->
    io_lib:format(
        "~nFatal error: unknown ~p for ~p in line ~p~n",
        [Unknown, Data, Line]).

-spec get_return(error_type()) -> term().

get_return(eparse) -> {ok, undefined};
get_return(_) -> {return, undefined}.

-spec get_level( Level :: pos_integer() ) -> binary().

get_level(?E_ERROR) -> <<"Fatal error">>;
get_level(?E_WARNING) -> <<"Warning">>;
get_level(?E_PARSE) -> <<"Parse error">>;
get_level(?E_NOTICE) -> <<"Notice">>;
get_level(?E_CORE_ERROR) -> <<"Error">>;
get_level(?E_CORE_WARNING) -> <<"Warning">>;
get_level(?E_COMPILE_ERROR) -> <<"Error">>;
get_level(?E_COMPILE_WARNING) -> <<"Warning">>;
get_level(?E_USER_ERROR) -> <<"Error">>;
get_level(?E_USER_WARNING) -> <<"Warning">>;
get_level(?E_USER_NOTICE) -> <<"Notice">>;
get_level(?E_STRICT) -> <<"Strict">>;
get_level(?E_RECOVERABLE_ERROR) -> <<"Catchable fatal error">>;
get_level(?E_DEPRECATED) -> <<"Deprecated">>;
get_level(?E_USER_DEPRECATED) -> <<"Deprecated">>;
get_level(_) -> <<"Unknown">>.

-spec get_line(line() | undefined) -> non_neg_integer() | undefined.

get_line(undefined) ->
    undefined;

get_line({{line, Line}, {column, _Column}}) ->
    Line.

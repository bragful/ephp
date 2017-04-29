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

-type error_format() :: text | html.

-type error_type() ::
    eundefclass |
    eprivateaccess |
    ecallprivate |
    eunsupportop |
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
    level = ?E_ALL,
    format = text :: error_format()
}).

-spec start_link() -> {ok, ephp:errors_id()}.

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, #state{}),
    {ok, Ref}.

destroy(Funcs) ->
    erlang:erase(Funcs),
    ok.

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
    ErrorState = erlang:get(ephp_context:get_errors_id(Context)),
    #state{format = Format} = ErrorState,
    ErrorText = get_message(Format, Type, Line, get_level(Level), Data),
    case ErrorState of
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

-spec get_message(error_format(), error_type(), pos_integer() | undefined,
                  binary(), binary() | term()) -> string().

get_message(text, Type, Line, Level, Args) ->
    io_lib:format(
        "~n~s: ~s on line ~p~n",
        [Level, get_message(Type, Args), Line]);

get_message(html, Type, Line, Level, Args) ->
    io_lib:format(
        "<br/>~n<strong>~s</strong>: ~s on line ~p<br/>~n",
        [Level, get_message(Type, Args), Line]).

-spec get_message(error_type(), binary() | term()) -> string().

get_message(eparse, Filename) ->
    io_lib:format("parse error in ~s", [Filename]);

get_message(erequired, {File, ReqFile, Func}) ->
    IncludePath = ephp_config:get(<<"include_path">>, <<".:">>),
    io_lib:format(
        "~s(): Failed opening required '~s'"
        " (include_path='~s') in ~s",
        [Func, ReqFile, IncludePath, File]);

get_message(einclude, {File, ReqFile, Func}) ->
    IncludePath = ephp_config:get(<<"include_path">>, <<".:">>),
    io_lib:format(
        "~s(): Failed opening '~s' for inclusion (include_path='~s') in ~s",
        [Func, ReqFile, IncludePath, File]);

get_message(enofile, {File, OpenFile, Func}) ->
    io_lib:format(
        "~s(~s): failed to open stream: No such file or directory in ~s",
        [Func, OpenFile, File]);

get_message(eundefun, {File, Fun}) ->
    io_lib:format("Call to undefined function ~s() in ~s", [Fun, File]);

get_message(eundefclass, {File, <<>>}) ->
    io_lib:format(
        "Cannot access self:: when no class scope is active in ~s",
        [File]);

get_message(eundefclass, {File, Class}) ->
    io_lib:format(
        "Class '~s' not found in ~s",
        [ephp_data:to_bin(Class), File]);

get_message(eprivateaccess, {File, Class, Element, Access}) ->
    io_lib:format(
        "Cannot access ~s property ~s::$~s in ~s",
        [Access, Class, Element, File]);

get_message(ecallprivate, {File, Class, Element, Access}) ->
    io_lib:format(
        "Call to ~s method ~s::~s() in ~s",
        [Access, Class, Element, File]);

get_message(eunsupportop, File) ->
    io_lib:format("Unsupported operand types in ~s", [File]);

get_message(eassignthis, File) ->
    io_lib:format("Cannot re-assign $this in ~s", [File]);

get_message(ewrongminarity, {Function, NumArgsExp, NumArgsSent, File}) ->
    io_lib:format(
        "~s() expects at least ~p parameters, ~p given in ~s",
        [Function, NumArgsExp, NumArgsSent, File]);

get_message(ewrongmaxarity, {Function, NumArgsExp, NumArgsSent, File}) ->
    io_lib:format(
        "~s() expects at most ~p parameters, ~p given in ~s",
        [Function, NumArgsExp, NumArgsSent, File]);

get_message(ewrongarg, {Function, ArgNum, ArgType, WrongType, File}) ->
    io_lib:format(
        "~s() expects parameter ~p to be ~s, ~s given in ~s",
        [Function, ArgNum, ArgType, WrongType, File]);

get_message(enoarray, File) ->
    io_lib:format("Cannot use a scalar value as an array in ~s", [File]);

get_message(einvalid, {Function, Spec, Val, File}) ->
    io_lib:format(
        "~s(): Invalid `~s' (~p) in ~s",
        [Function, Spec, Val, File]);

get_message(eundefvar, {File, Var}) ->
    io_lib:format("Undefined variable: ~s in ~s", [Var, File]);

get_message(eundefconst, {File, Const}) ->
    io_lib:format(
        "Use of undefined constant ~s - assumed '~s' in ~s",
        [Const, Const, File]);

get_message(enotostring, {File, ClassName}) ->
    io_lib:format(
        "Object of class ~s could not be converted to string in ~s",
        [ClassName, File]);

get_message(enocast, {File, ClassName, Type}) ->
    io_lib:format(
        "Object of class ~s could not be converted to ~s in ~s",
        [ClassName, Type, File]);

get_message(earrayconv, {File, Type}) ->
    io_lib:format("Array to ~s conversion in ~s", [Type, File]);

get_message(eredefinedclass, {File, ClassName}) ->
    io_lib:format(
        "Cannot redeclare class ~s in ~s",
        [ClassName, File]);

get_message(edivzero, File) ->
    io_lib:format("Division by zero in ~s", [File]);

get_message(etimezone, {Function, TZ, File}) ->
    io_lib:format(
        "~s(): Timezone ID '~s' is invalid in ~s",
        [Function, TZ, File]);

get_message(efewargs, {Function, File}) ->
    io_lib:format("~s(): Too few arguments in ~s", [Function, File]);

get_message(eargsupplied, {Function, File}) ->
    io_lib:format("Invalid argument supplied for ~s() in ~s", [Function, File]);

get_message(eargtype, {Function, ArgNum, File}) ->
    io_lib:format(
        "~s(): Argument #~p is not an array in ~s",
        [Function, ArgNum, File]);

get_message(eisnot, {Function, VarName, Type, File}) ->
    io_lib:format(
        "~s(): ~s is not ~s in ~s",
        [Function, VarName, Type, File]);

get_message(eoffset, {Function, File}) ->
    io_lib:format(
        "~s(): Offset not contained in string in ~s",
        [Function, File]);

get_message(Unknown, Data) ->
    io_lib:format("unknown ~p for ~p", [Unknown, Data]).

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

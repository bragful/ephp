-module(ephp_error).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    start_link/0,
    destroy/1,

    set_output/2,
    set_output_handler/2,
    add_message_handler/2,

    run_quiet/2,

    error_reporting/2,
    set_error_format/2,
    error/1,
    handle_error/2,
    get_line/1,
    get_level/1
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
    format = text :: error_format(),
    modules = [] :: [module()]
}).

-spec start_link() -> {ok, ephp:errors_id()}.

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, #state{}),
    {ok, Ref}.

-spec destroy(ephp:errors_id()) -> ok.

destroy(ErrorsId) ->
    erlang:erase(ErrorsId),
    ok.

-spec add_message_handler(ephp:context(), module()) -> ok.

add_message_handler(Context, Module) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    #state{modules = Modules} = State = erlang:get(ErrorsId),
    erlang:put(ErrorsId, State#state{modules = [Module|Modules]}),
    ok.

-spec set_error_format(context(), error_format()) -> ok.

set_error_format(Context, Format) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    erlang:put(ErrorsId, State#state{format = Format}),
    ok.

-spec error_reporting(context(), integer()) -> integer().

error_reporting(Context, Level) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    erlang:put(ErrorsId, State#state{level = Level}),
    State#state.level.

-type throw_error() ::
    atom() |
    {error, error_type(), line(), error_level(), any()}.

-spec error(throw_error()) -> ok.

error({error, Type, Index, Level, Data}) ->
    throw({error, Type, Index, Level, Data}).

-spec handle_error(context(), {error, error_type(), line(), binary(),
    error_level(), any()}) -> ok.

handle_error(Context, {error, Type, Index, File, Level, Data}) ->
    Line = get_line(Index),
    ErrorState = erlang:get(ephp_context:get_errors_id(Context)),
    #state{format = Format, modules = Modules} = ErrorState,
    ErrorText = get_message(Modules, Format, Type, Line, File, Level, Data),
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

-spec get_message([module()], error_format(), error_type(),
                  pos_integer() | undefined, binary(), binary(),
                  term()) -> string().

get_message(Modules, text, Type, Line, File, Level, Args) ->
    Message = get_message(Modules, Type, Level, Args),
    io_lib:format(
        "~n~s: ~s in ~s on line ~p~n",
        [get_level(Level), Message, File, Line]);

get_message(Modules, html, Type, Line, File, Level, Args) ->
    Message = get_message(Modules, Type, Level, Args),
    io_lib:format(
        "<br/>~n<strong>~s</strong>: ~s in ~s on line ~p<br/>~n",
        [get_level(Level), Message, File, Line]).

-spec get_message([module()], error_type(), pos_integer(), term()) -> string().

get_message([], Type, _Level, Args) ->
    get_message(Type, Args);

get_message([Module|Modules], Type, Level, Args) ->
    case Module:handle_error(Type, Level, Args) of
        Message when is_list(Message) -> Message;
        ignore -> get_message(Modules, Type, Level, Args)
    end.

-spec get_message(error_type(), binary() | term()) -> string().

get_message(eparse, {}) ->
    "parse error";

get_message(enofile, {OpenFile, Func}) ->
    io_lib:format(
        "~s(~s): failed to open stream: No such file or directory",
        [Func, OpenFile]);

get_message(eundefun, {Fun}) ->
    io_lib:format("Call to undefined function ~s()", [Fun]);

get_message(eunsupportop, {}) ->
    "Unsupported operand types";

get_message(ewrongminarity, {Function, NumArgsExp, NumArgsSent}) ->
    io_lib:format(
        "~s() expects at least ~p parameters, ~p given",
        [Function, NumArgsExp, NumArgsSent]);

get_message(ewrongmaxarity, {Function, NumArgsExp, NumArgsSent}) ->
    io_lib:format(
        "~s() expects at most ~p parameters, ~p given",
        [Function, NumArgsExp, NumArgsSent]);

get_message(ewrongarg, {Function, ArgNum, ArgType, WrongType}) ->
    io_lib:format(
        "~s() expects parameter ~p to be ~s, ~s given",
        [Function, ArgNum, ArgType, WrongType]);

get_message(enoarray, {}) ->
    "Cannot use a scalar value as an array";

get_message(einvalid, {Function, Spec, Val}) ->
    io_lib:format("~s(): Invalid `~s' (~p)", [Function, Spec, Val]);

get_message(eundefvar, {Var}) ->
    io_lib:format("Undefined variable: ~s", [Var]);

get_message(eundefconst, {Const}) ->
    io_lib:format(
        "Use of undefined constant ~s - assumed '~s'",
        [Const, Const]);

get_message(enotostring, {ClassName}) ->
    io_lib:format(
        "Object of class ~s could not be converted to string",
        [ClassName]);

get_message(enocast, {ClassName, Type}) ->
    io_lib:format(
        "Object of class ~s could not be converted to ~s",
        [ClassName, Type]);

get_message(earrayconv, {Type}) ->
    io_lib:format("Array to ~s conversion", [Type]);

get_message(edivzero, {}) ->
    "Division by zero";

get_message(efewargs, {Function}) ->
    io_lib:format("~s(): Too few arguments", [Function]);

get_message(eargsupplied, {Function}) ->
    io_lib:format("Invalid argument supplied for ~s()", [Function]);

get_message(eargtype, {Function, ArgNum}) ->
    io_lib:format("~s(): Argument #~p is not an array", [Function, ArgNum]);

get_message(eisnot, {Function, VarName, Type}) ->
    io_lib:format("~s(): ~s is not ~s", [Function, VarName, Type]);

get_message(eoffset, {Function}) ->
    io_lib:format("~s(): Offset not contained in string", [Function]);

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

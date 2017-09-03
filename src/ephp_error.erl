-module(ephp_error).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    start_link/0,
    destroy/1,

    set_output/2,
    set_output_handler/2,
    add_message_handler/2,

    set_exception_handler_func/2,
    get_exception_handler_func/1,
    remove_exception_handler_func/1,

    set_error_handler_func/3,
    get_error_handler_func/1,
    remove_error_handler_func/1,

    get_last/1,
    clear_last/1,

    run_quiet/2,

    error_reporting/2,
    error/1,
    handle_error/2,
    get_line/1,
    get_level/1
]).

-callback set_output(context(), string()) -> ok.

-include("ephp.hrl").

-type error_format() :: text | html.

-type error_type() :: atom().

-record(state, {
    ref :: ephp:errors_id(),
    output_handler = ?MODULE :: module(),
    error_handler = [] :: [{callable(), non_neg_integer()}],
    exception_handler :: callable(),
    silent = false :: boolean(),
    level = ?E_ALL band (bnot ?E_STRICT) :: non_neg_integer(),
    modules = [] :: [module()],
    last_error :: ephp_array() | undefined
}).

-spec start_link() -> {ok, ephp:errors_id()}.

start_link() ->
    Ref = make_ref(),
    erlang:put(Ref, #state{ref = Ref}),
    {ok, Ref}.

-spec destroy(ephp:errors_id()) -> ok.

destroy(ErrorsId) ->
    erlang:erase(ErrorsId),
    ok.

-spec add_message_handler(context(), module()) -> ok.

add_message_handler(Context, Module) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    #state{modules = Modules} = State = erlang:get(ErrorsId),
    erlang:put(ErrorsId, State#state{modules = [Module|Modules]}),
    ok.

-spec set_exception_handler_func(context(), callable()) -> ok.

set_exception_handler_func(Context, Callable) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    erlang:put(ErrorsId, State#state{exception_handler = Callable}),
    ok.

-spec get_exception_handler_func(context()) -> callable() | undefined.

get_exception_handler_func(Context) ->
    ErrorState = erlang:get(ephp_context:get_errors_id(Context)),
    ErrorState#state.exception_handler.

-spec remove_exception_handler_func(context()) -> ok.

remove_exception_handler_func(Context) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    erlang:put(ErrorsId, State#state{exception_handler = undefined}),
    ok.

-spec set_error_handler_func(context(), callable(), non_neg_integer()) -> ok.

set_error_handler_func(Context, Callable, ErrorLevel) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    ErrorHandlers = [{Callable, ErrorLevel}|State#state.error_handler],
    erlang:put(ErrorsId, State#state{error_handler = ErrorHandlers}),
    ok.

-spec get_error_handler_func(context()) -> callable() | undefined.

get_error_handler_func(Context) ->
    ErrorState = erlang:get(ephp_context:get_errors_id(Context)),
    case ErrorState#state.error_handler of
        [] -> undefined;
        [ErrorHandler|_] -> ErrorHandler
    end.

-spec remove_error_handler_func(context()) -> ok.

remove_error_handler_func(Context) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    case State#state.error_handler of
        [] ->
            ok;
        [_|ErrorHandlers] ->
            erlang:put(ErrorsId, State#state{error_handler = ErrorHandlers}),
            ok
    end.

-spec get_last(context()) -> ephp_array() | undefined.

get_last(Context) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    State#state.last_error.

-spec clear_last(context()) -> ok.

clear_last(Context) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    erlang:put(ErrorsId, State#state{last_error = undefined}),
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

handle_error(Context, {error, euncaught, Index, File, Level,
                       {File, Line, Exception}}) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    case erlang:get(ErrorsId) of
        #state{exception_handler = undefined,
               modules = Modules,
               output_handler = OutputHandler} ->
            Format = get_format(),
            {_, ErrorText} = get_message(Modules, Format, euncaught, Line, File,
                                         Level, {File, Line, Exception}),
            OutputHandler:set_output(Context, iolist_to_binary(ErrorText));
        #state{exception_handler = ExceptionHandler} ->
            Call = #call{name = ExceptionHandler,
                         args = [Exception],
                         line = Index},
            ephp_context:solve(Context, Call)
    end,
    get_return(euncaught);

handle_error(Context, {error, _Type, Index, File, Level, _Data}) when
        Level =:= 0 orelse Level band (bnot ?E_ALL) =/= 0 ->
    handle_error(Context, {error, enolevel, Index, File, ?E_WARNING, {}});

handle_error(Context, {error, Type, Index, File, Level, Data}) ->
    Line = get_line(Index),
    ErrorsId = ephp_context:get_errors_id(Context),
    ErrorState = erlang:get(ErrorsId),
    #state{modules = Modules} = ErrorState,
    Format = get_format(),
    {SimpleErrText, ErrorText} =
        get_message(Modules, Format, Type, Line, File, Level, Data),
    if
        Type =/= euncaught ->
            LastError = ephp_array:from_list([
                {<<"type">>, Level},
                {<<"message">>, iolist_to_binary(SimpleErrText)},
                {<<"file">>, File},
                {<<"line">>, Line}
            ]),
            erlang:put(ErrorsId, ErrorState#state{last_error = LastError});
        true ->
            ok
    end,
    case ErrorState of
        #state{error_handler = [{ErrHandler, CfgLevel}|_], output_handler = Module}
                when (CfgLevel band Level) =/= 0
                andalso (?E_HANDLE_ERRORS band Level) =/= 0 ->
            case run(Context, ErrorState, ErrHandler, Level, SimpleErrText,
                     File, Index) of
                false ->
                    Module:set_output(Context, iolist_to_binary(ErrorText)),
                    if
                        (?E_EXIT_ON_FALSE band Level) =/= 0 ->
                            throw({ok, die});
                        true -> ok
                    end;
                _ ->
                    ok
            end;
        #state{silent = true} ->
            ok;
        #state{level = CfgLevel} when (CfgLevel band Level) =:= 0 ->
            ok;
        #state{output_handler = Module} ->
            Module:set_output(Context, iolist_to_binary(ErrorText)),
            if
                (?E_EXIT_ON_FALSE band Level) =/= 0 ->
                   throw({ok, die});
                true -> ok
            end
    end,
    get_return(Type).

-spec run(context(), #state{}, callable(), integer(), string(), binary(),
          line()) -> boolean().

run(Context, State, ErrHandler, Level, ErrorText, File, Index) ->
    erlang:put(State#state.ref, State#state{error_handler = []}),
    Args = [#int{int = Level}, #text{text = iolist_to_binary(ErrorText)},
            #text{text = File}, #int{int = get_line(Index)}],
    Call = #call{name = ErrHandler, args = Args, line = Index},
    Return = case ephp_context:solve(Context, Call) of
        false -> false;
        _ -> true
    end,
    case (erlang:get(State#state.ref))#state.error_handler of
        [] -> erlang:put(State#state.ref, State);
        _ -> ok
    end,
    Return.

-spec set_output(context(), binary()) -> ok.

set_output(Context, Text) ->
    ephp_context:set_output(Context, Text).

-spec set_output_handler(context(), module()) -> ok.

set_output_handler(Context, Module) ->
    ErrorsId = ephp_context:get_errors_id(Context),
    State = erlang:get(ErrorsId),
    erlang:put(ErrorsId, State#state{output_handler = Module}),
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
                  term()) -> {string(), string()}.

get_message(Modules, text, Type, Line, File, Level, Args) ->
    Message = get_message(Modules, Type, Level, Args),
    {Message, io_lib:format(
        "~n~s: ~s in ~s on line ~p~n",
        [get_level(Level), Message, File, Line])};

get_message(Modules, html, Type, Line, File, Level, Args) ->
    Message = get_message(Modules, Type, Level, Args),
    {Message, io_lib:format(
        "<br/>~n<b>~s</b>: ~s in <b>~s</b> on line <b>~p</b><br/>~n",
        [get_level(Level), Message, File, Line])}.

-spec get_message([module()], error_type(), pos_integer(), term()) -> string().

get_message([], Type, _Level, Args) ->
    get_message(Type, Args);

get_message([Module|Modules], Type, Level, Args) ->
    case Module:handle_error(Type, Level, Args) of
        Message when is_list(Message) -> Message;
        ignore -> get_message(Modules, Type, Level, Args)
    end.

-spec get_message(error_type(), binary() | term()) -> string().

get_message(eparse, _Rest) when is_binary(_Rest) ->
    "parse error";

get_message(eparse, {unexpected, Value}) when is_binary(Value) ->
    io_lib:format("syntax error, unexpected '~s'", [Value]);

get_message(eparse, {Value, Type}) when is_binary(Type) ->
    io_lib:format("syntax error, unexpected '~s' (~s)", [Value, Type]);

get_message(eparse, {Type}) when is_binary(Type) ->
    io_lib:format("parse error, expecting ~s", [Type]);

get_message(trigger, {ErrStr}) when is_binary(ErrStr) ->
    ErrStr;

get_message(enolevel, {}) ->
    "Invalid error type specified";

get_message(enostrfunc, {}) ->
    "Function name must be a string";

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

get_message(eshouldbe, {Function, Key, Type}) ->
    io_lib:format("~s(): ~s should be ~s", [Function, Key, Type]);

get_message(eoffset, {Function}) ->
    io_lib:format("~s(): Offset not contained in string", [Function]);

get_message(enorefvar, {}) ->
    io_lib:format("Only variables can be passed by reference", []);

get_message(euncaught, {File, Line, Exception}) ->
    StackTrace = ephp_class_exception:get_trace(Exception),
    Traces = lists:map(fun trace_to_str/1, ephp_array:to_list(StackTrace)),
    Message = ephp_class_exception:get_message(Exception),
    MessageFmt = case Message of
        <<>> -> "~s";
        _ -> " with message '~s'"
    end,
    ClassName = ephp_object:get_class_name(Exception),
    io_lib:format(
        "Uncaught exception '~s'" ++ MessageFmt ++
        " in ~s:~p~nStack trace:~n~s#~p {main}~n  thrown",
        [ClassName, Message, File, Line, Traces, ephp_array:size(StackTrace)]);

get_message(enoobjectexception, {}) ->
    io_lib:format("Can only throw objects", []);

get_message(errtype, {I, ReqType, GivenType, FuncName}) ->
    io_lib:format("Argument ~p passed to ~s() must be an instance of ~s, "
                  "~s given, called", [I, FuncName, ReqType, GivenType]);

get_message(einvalidcallback, {Func, Arg}) ->
    io_lib:format("~s() expects the argument (~s) to be a valid callback",
                  [Func, Arg]);

get_message(Unknown, Data) ->
    io_lib:format("unknown ~p for ~p", [Unknown, Data]).

-spec trace_to_str({pos_integer(), ephp_array()}) -> iolist().

trace_to_str({I, Array}) ->
    {ok, FuncName} = ephp_array:find(<<"function">>, Array),
    {ok, Line} = ephp_array:find(<<"line">>, Array),
    {ok, File} = ephp_array:find(<<"file">>, Array),
    {ok, RawArgList} = ephp_array:find(<<"args">>, Array),
    ArgStrList = lists:map(fun
        ({_, #var_ref{pid = Vars, ref = Var}}) ->
            binary_to_list(ephp_string:escape(ephp_vars:get(Vars, Var), $'));
        ({_, Value}) when is_binary(Value) ->
            binary_to_list(ephp_string:escape(Value, $'));
        ({_, Value}) when is_number(Value) ->
            binary_to_list(ephp_data:to_bin(Value))
    end, ephp_array:to_list(RawArgList)),
    Args = string:join(ArgStrList, ","),
    case File of
        undefined ->
            io_lib:format(
                "#~p [internal function]: ~s(~s)~n", [I, FuncName, Args]);
        _ ->
            io_lib:format(
                "#~p ~s(~p): ~s(~s)~n", [I, File, Line, FuncName, Args])
    end.

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
get_level(?E_USER_ERROR) -> <<"Fatal error">>;
get_level(?E_USER_WARNING) -> <<"Warning">>;
get_level(?E_USER_NOTICE) -> <<"Notice">>;
get_level(?E_STRICT) -> <<"Strict Standards">>;
get_level(?E_RECOVERABLE_ERROR) -> <<"Catchable fatal error">>;
get_level(?E_DEPRECATED) -> <<"Deprecated">>;
get_level(?E_USER_DEPRECATED) -> <<"Deprecated">>;
get_level(_) -> <<"Unknown">>.

-spec get_format() -> error_format().

get_format() ->
    case ephp_config:get_bool(<<"html_errors">>, false) of
        true -> html;
        false -> text
    end.

-spec get_line(line() | undefined) -> non_neg_integer() | undefined.

get_line(undefined) ->
    undefined;

get_line({{line, Line}, {column, _Column}}) ->
    Line.

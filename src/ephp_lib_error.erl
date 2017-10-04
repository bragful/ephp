-module(ephp_lib_error).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,

    debug_backtrace/4,
    debug_print_backtrace/4,
    error_reporting/3,
    set_error_handler/4,
    restore_error_handler/2,
    set_exception_handler/3,
    restore_exception_handler/2,
    error_get_last/2,
    error_clear_last/2,
    trigger_error/4
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {debug_backtrace, [
        {args, {0, 2, undefined, [{str_or_int, 1}, {str_or_int, 0}]}}
    ]},
    {debug_print_backtrace, [
        {args, {0, 2, undefined, [{str_or_int, 0}, {str_or_int, 0}]}}
    ]},
    {set_error_handler, [
        {args, {1, 2, undefined, [mixed, {integer, ?E_ALL bor ?E_STRICT}]}}
    ]},
    restore_error_handler,
    set_exception_handler,
    restore_exception_handler,
    error_get_last,
    error_clear_last,
    error_reporting,
    {trigger_error, [
        {args, {1, 2, false, [string, {integer, ?E_USER_NOTICE}]}}
    ]},
    {trigger_error, [
        {alias, <<"user_error">>},
        {args, {1, 2, false, [string, {integer, ?E_USER_NOTICE}]}}
    ]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [
    {<<"E_ERROR">>, ?E_ERROR},
    {<<"E_WARNING">>, ?E_WARNING},
    {<<"E_PARSE">>, ?E_PARSE},
    {<<"E_NOTICE">>, ?E_NOTICE},
    {<<"E_CORE_ERROR">>, ?E_CORE_ERROR},
    {<<"E_CORE_WARNING">>, ?E_CORE_WARNING},
    {<<"E_COMPILE_ERROR">>, ?E_COMPILE_ERROR},
    {<<"E_COMPILE_WARNING">>, ?E_COMPILE_WARNING},
    {<<"E_USER_ERROR">>, ?E_USER_ERROR},
    {<<"E_USER_WARNING">>, ?E_USER_WARNING},
    {<<"E_USER_NOTICE">>, ?E_USER_NOTICE},
    {<<"E_STRICT">>, ?E_STRICT},
    {<<"E_RECOVERABLE_ERROR">>, ?E_RECOVERABLE_ERROR},
    {<<"E_DEPRECATED">>, ?E_DEPRECATED},
    {<<"E_USER_DEPRECATED">>, ?E_USER_DEPRECATED},
    {<<"E_ALL">>, ?E_ALL},
    {<<"DEBUG_BACKTRACE_PROVIDE_OBJECT">>, ?DEBUG_BACKTRACE_PROVIDE_OBJECT},
    {<<"DEBUG_BACKTRACE_IGNORE_ARGS">>, ?DEBUG_BACKTRACE_IGNORE_ARGS}
].

-spec debug_backtrace(context(), line(), var_value(), var_value()) ->
      ephp_array().

debug_backtrace(Context, _Line, {_, _Flags}, {_, _Limit}) ->
    ephp_stack:get_array(Context, 1).

-spec debug_print_backtrace(context(), line(),
                            var_value(), var_value()) -> undefined.

debug_print_backtrace(Context, _Line, {_, IncludeArgs}, {_, Limit}) ->
    Backtrace = ephp_stack:get_array(Context, 1),
    ephp_array:fold(fun
        (I, Data, _) when Limit =:= 0 orelse I < Limit ->
            Str = iolist_to_binary(trace_to_str(I, Data, Context, IncludeArgs)),
            ephp_context:set_output(Context, Str);
        (_, _, _) ->
            ok
    end, [], Backtrace),
    undefined.

-spec trace_to_str(pos_integer(), ephp_array(), context(), pos_integer()) ->
      iolist().

trace_to_str(I, Array, Ctx, IncludeArgs) ->
    {ok, FuncName} = ephp_array:find(<<"function">>, Array),
    {ok, Line} = ephp_array:find(<<"line">>, Array),
    {ok, File} = ephp_array:find(<<"file">>, Array),
    {ok, RawArgList} = ephp_array:find(<<"args">>, Array),
    IncArgs = ephp_data:to_int(IncludeArgs),
    Args = if
        (IncArgs band ?DEBUG_BACKTRACE_IGNORE_ARGS) =:= 0 ->
            ArgStrList = lists:map(fun({_, #var_ref{pid = Vars, ref = Var}}) ->
                Value = ephp_vars:get(Vars, Var, Ctx),
                binary_to_list(ephp_string:escape(Value, $'))
            end, ephp_array:to_list(RawArgList)),
            string:join(ArgStrList, ",");
        true ->
            ""
    end,
    io_lib:format(
        "#~p  ~s(~s) called at [~s:~p]~n", [I, FuncName, Args, File, Line]).

-spec error_reporting(context(), line(), var_value()) -> integer().

error_reporting(Context, _Line, {_, ErrorLevel}) when is_integer(ErrorLevel) ->
    ephp_error:error_reporting(Context, ErrorLevel).

-spec set_error_handler(context(), line(), var_value(), var_value()) -> mixed().

set_error_handler(Context, _Line, {_, #function{}=ErrorHandler}, {_, ErrorLevel}) ->
    case ephp_error:get_error_handler_func(Context) of
        {OldErrorHandler, _} ->
            OldErrorHandler;
        undefined ->
            OldErrorHandler = undefined
    end,
    ephp_error:set_error_handler_func(Context, ErrorHandler, ErrorLevel),
    OldErrorHandler;

set_error_handler(Context, Line, {_, ErrorHandler}, {_, ErrorLevel}) ->
    %% FIXME: ErrorHandler maybe could be a callable instead...
    case ephp_context:is_defined_function(Context, ErrorHandler) of
        true ->
            case ephp_error:get_error_handler_func(Context) of
                {OldErrorHandler, _} ->
                    OldErrorHandler;
                undefined ->
                    OldErrorHandler = undefined
            end,
            ephp_error:set_error_handler_func(Context, ErrorHandler, ErrorLevel),
            OldErrorHandler;
        false ->
            Data = {<<"set_error_handler">>, ephp_data:to_bin(ErrorHandler)},
            File = ephp_context:get_active_file(Context),
            Error = {error, einvalidcallback, Line, File, ?E_WARNING, Data},
            ephp_error:handle_error(Context, Error),
            undefined
    end.

-spec restore_error_handler(context(), line()) -> true.

restore_error_handler(Context, _Line) ->
    ephp_error:remove_error_handler_func(Context),
    true.

-spec set_exception_handler(context(), line(), var_value()) -> callable().

set_exception_handler(Context, Line, {_, ExceptionHandler}) ->
    %% FIXME: ExceptionHandler maybe could be a callable instead...
    case ephp_context:is_defined_function(Context, ExceptionHandler) of
        true ->
            OldExceptionHandler = ephp_error:get_exception_handler_func(Context),
            ephp_error:set_exception_handler_func(Context, ExceptionHandler),
            OldExceptionHandler;
        false ->
            Data = {<<"set_exception_handler">>, ephp_data:to_bin(ExceptionHandler)},
            File = ephp_context:get_active_file(Context),
            Error = {error, einvalidcallback, Line, File, ?E_WARNING, Data},
            ephp_error:handle_error(Context, Error),
            undefined
    end.

-spec restore_exception_handler(context(), line()) -> true.

restore_exception_handler(Context, _Line) ->
    ephp_error:remove_exception_handler_func(Context),
    true.

-spec error_get_last(context(), line()) -> ephp_array() | undefined.

error_get_last(Context, _Line) ->
    ephp_error:get_last(Context).

-spec error_clear_last(context(), line()) -> undefined.

error_clear_last(Context, _Line) ->
    ephp_error:clear_last(Context),
    undefined.

-spec trigger_error(context(), line(), ErrStr :: var_value(),
                    ErrLevel :: var_value()) -> boolean().

trigger_error(Context, Line, _ErrStr, {_, ErrLevel})
        when ErrLevel band (bnot ?E_USER) =/= 0 ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context,
                            {error, enolevel, Line, File, ?E_WARNING, {}});
trigger_error(Context, Line, {_, ErrStr}, {_, ErrLevel}) ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context, {error, trigger, Line, File, ErrLevel,
                                      {ErrStr}}).


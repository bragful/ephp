-module(ephp_error).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    error/1,
    handle_error/2
]).

-include("ephp.hrl").

-type error_type() ::
    eundefclass |
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
    ephp_context:set_output(Context, iolist_to_binary(ErrorText)),
    get_return(Type).

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

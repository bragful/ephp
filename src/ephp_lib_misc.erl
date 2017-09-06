-module(ephp_lib_misc).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    define/4,
    defined/3,
    sleep/3,
    usleep/3,
    exit/3,
    pack/3
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    define,
    defined,
    sleep,
    usleep,
    {exit, [{alias, <<"die">>}]},
    exit,
    {pack, [pack_args]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].

-spec define(context(), line(), Constant :: var_value(),
    Content :: var_value()) -> boolean().

define(Context, _Line, {#text{},Constant}, {_,Content}) ->
    ephp_context:register_const(Context, Constant, Content),
    true;

define(Context, _Line, {#constant{name=Constant},_},
        {_UnParsedContent,Content}) ->
    ephp_context:register_const(Context, Constant, Content),
    true.

-spec defined(context(), line(), var_value()) -> boolean().

defined(Context, _Line, {_, ConstantName}) ->
    case ephp_context:get_const(Context, ConstantName, false) of
        false -> false;
        _ -> true
    end.

-spec sleep(context(), line(), Seconds :: var_value()) -> false | integer().

sleep(_Context, _Line, {_, Seconds}) when is_number(Seconds) ->
    timer:sleep(trunc(Seconds) * 1000),
    0;

sleep(Context, Line, {_, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"sleep">>, 1, <<"long">>, ephp_data:gettype(Val)},
    ephp_error:handle_error(Context, {error, ewrongarg, Line, File,
        ?E_WARNING, Data}),
    false.

-spec usleep(context(), line(), MicroSeconds :: var_value()) ->
    false | integer().

usleep(_Context, _Line, {_, MicroSeconds}) when is_number(MicroSeconds) ->
    timer:sleep(trunc(MicroSeconds) div 1000),
    0;

usleep(Context, Line, {_, Val}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"usleep">>, 1, <<"long">>, ephp_data:gettype(Val)},
    ephp_error:handle_error(Context, {error, ewrongarg, Line, File,
        ?E_WARNING, Data}),
    false.

-spec exit(context(), line(), Message :: var_value()) ->
    undefined.

exit(Context, _Line, {_, Value}) ->
    ephp_context:set_output(Context, Value),
    throw(die).

-spec pack(context(), line(), [var_value()]) -> binary().

pack(_Context, _Line, [{_, Format}|Args]) ->
    do_pack(ephp_data:to_bin(Format), [ A || {_, A} <- Args ], <<>>).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

do_pack(<<>>, _, Binary) ->
    Binary;

do_pack(<<A:8,"*", Rest/binary>>, [Arg|Args], Binary) when A =:= $A orelse
                                                           A =:= $a ->
    String = ephp_data:to_bin(Arg),
    do_pack(Rest, Args, <<Binary/binary, String/binary>>);

do_pack(<<"a", Rest/binary>>, [Arg|Args], Binary) ->
    {Size, Rest0} = get_numbers(Rest),
    String = ephp_data:to_bin(Arg),
    Size0 = Size,
    String0 = case Size0 - byte_size(String) of
        N when N =:= 0 ->
            <<Binary/binary, String/binary>>;
        N when N < 0 ->
            <<Binary/binary, String:Size0/binary>>;
        N when N > 0 ->
            Nulls = ephp_string:repeat(N, 0),
            <<Binary/binary, String/binary, Nulls/binary>>
    end,
    do_pack(Rest0, Args, String0);

do_pack(<<"A", Rest/binary>>, [Arg|Args], Binary) ->
    {Size, Rest0} = get_numbers(Rest),
    String = ephp_data:to_bin(Arg),
    Size0 = if Size =:= 0 -> 1; true -> Size end,
    String0 = case Size0 - byte_size(String) of
        N when N =:= 0 ->
            <<Binary/binary, String/binary>>;
        N when N < 0 ->
            <<Binary/binary, String:Size0/binary>>;
        N when N > 0 ->
            Spaces = ephp_string:spaces(N),
            <<Binary/binary, String/binary, Spaces/binary>>
    end,
    do_pack(Rest0, Args, String0).

-spec get_numbers(binary()) -> {non_neg_integer(), binary()}.
%% @doc retrieve numbers from binary while it's possible.
get_numbers(Binary) ->
    get_numbers(Binary, <<>>).

-spec get_numbers(binary(), binary()) -> {non_neg_integer(), binary()}.
%% @private
get_numbers(<<>>, <<>>) ->
    {0, <<>>};
get_numbers(<<>>, Num) ->
    {binary_to_integer(Num), <<>>};
get_numbers(<<A:8,Rest/binary>>, Num) when A >= $0 andalso A =< $9 ->
    get_numbers(Rest, <<Num/binary, A:8>>);
get_numbers(Rest, <<>>) ->
    {0, Rest};
get_numbers(Rest, Num) ->
    {binary_to_integer(Num), Rest}.

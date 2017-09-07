-module(ephp_lib_misc).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,
    handle_error/3,
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


-spec handle_error(ephp_error:error_type(), ephp_error:error_level(),
                   Args::term()) -> string() | ignore.

handle_error(eargunused, _Level, {Function, NumArgs}) ->
    io_lib:format("~s(): ~p arguments unused", [Function, NumArgs]);

handle_error(efewargs, _Level, {Function, Arg}) ->
    io_lib:format("~s(): Type ~s: too few arguments", [Function, Arg]);

handle_error(_Type, _Level, _Data) ->
    ignore.


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

pack(Context, Line, [{_, Format}|Args]) ->
    do_pack(Context, Line,
            ephp_data:to_bin(Format), [ A || {_, A} <- Args ], <<>>).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

do_pack(_Context, _Line, <<>>, [], Binary) ->
    Binary;

do_pack(Context, Line, <<>>, Args, Binary) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"pack">>, length(Args)},
    ephp_error:handle_error(Context, {error, eargunused, Line, File,
                                      ?E_WARNING, Data}),
    Binary;

do_pack(Context, Line, <<A:8,"*", Rest/binary>>, [Arg|Args], Binary)
        when A =:= $A orelse A =:= $a ->
    String = ephp_data:to_bin(Arg),
    do_pack(Context, Line, Rest, Args, <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"a", Rest/binary>>, [Arg|Args], Binary) ->
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
    do_pack(Context, Line, Rest0, Args, String0);

do_pack(Context, Line, <<"A", Rest/binary>>, [Arg|Args], Binary) ->
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
    do_pack(Context, Line, Rest0, Args, String0);

do_pack(Context, Line, <<"H*", Rest/binary>>, [Arg|Args], Binary) ->
    String = ephp_string:hex2bin(Arg),
    do_pack(Context, Line, Rest, Args, <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"H", Rest/binary>>, [Arg|Args], Binary) ->
    {Size, Rest0} = get_numbers(Rest),
    Size0 = if Size =:= 0 -> 1; true -> Size end,
    ArgStr = ephp_data:to_bin(Arg),
    String = case Size0 - byte_size(ArgStr) of
        N when N =:= 0 ->
            ephp_string:hex2bin(ArgStr);
        N when N < 0 ->
            <<ArgStr0:Size0/binary,_/binary>> = ArgStr,
            ephp_string:hex2bin(ArgStr0);
        N when N > 0 ->
            %% TODO: warning
            ephp_string:hex2bin(ArgStr)
    end,
    do_pack(Context, Line, Rest0, Args, <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"h*", Rest/binary>>, [Arg|Args], Binary) ->
    String = ephp_string:ihex2bin(Arg),
    do_pack(Context, Line, Rest, Args, <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"h", Rest/binary>>, [Arg|Args], Binary) ->
    {Size, Rest0} = get_numbers(Rest),
    Size0 = if Size =:= 0 -> 1; true -> Size end,
    ArgStr = ephp_data:to_bin(Arg),
    String = case Size0 - byte_size(ArgStr) of
        N when N =:= 0 ->
            ephp_string:ihex2bin(ArgStr);
        N when N < 0 ->
            <<ArgStr0:Size0/binary,_/binary>> = ArgStr,
            ephp_string:ihex2bin(ArgStr0);
        N when N > 0 ->
            %% TODO: warning
            ephp_string:ihex2bin(ArgStr)
    end,
    do_pack(Context, Line, Rest0, Args, <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"c*", Rest/binary>>, Args, Binary) ->
    String = lists:foldl(fun
        (Arg, S) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            <<S/binary, A:8>>;
        (_, S) ->
            <<S/binary, 0:8>>
    end, <<>>, Args),
    do_pack(Context, Line, Rest, [], <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"c", Rest/binary>>, Args, Binary) ->
    {Size, Rest0} = get_numbers(Rest),
    Size0 = if Size =:= 0 -> 1; true -> Size end,
    {NSize, NArgs, String} = lists:foldl(fun
        (Arg, {0, AA, Str}) ->
            {0, AA ++ [Arg], Str};
        (Arg, {Si, AA, Str}) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            {Si-1, AA, <<Str/binary, A:8>>};
        (_, {Si, AA, Str}) ->
            {Si-1, AA, <<Str/binary, 0:8>>}
    end, {Size0, [], <<>>}, Args),
    case NSize of
        0 ->
            do_pack(Context, Line, Rest0, NArgs,
                    <<Binary/binary, String/binary>>);
        _ ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"pack">>, <<"c">>},
            ephp_error:handle_error(Context, {error, efewargs, Line, File,
                                              ?E_WARNING, Data}),
            false
    end.


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

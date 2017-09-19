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
    pack/3,
    unpack/4
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    define,
    defined,
    sleep,
    usleep,
    {exit, [
        {args, {0, 1, undefined, [{mixed, undefined}]}},
        {alias, <<"die">>}
    ]},
    {exit, [{args, {0, 1, undefined, [{mixed, undefined}]}}]},
    {pack, [pack_args]},
    {unpack, [
        {args, {2, 2, undefined, [string, string]}}
    ]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].


-spec init_const() -> ephp_func:php_const_results().

init_const() -> [].


-spec handle_error(ephp_error:error_type(), ephp_error:error_level(),
                   Args::term()) -> string() | ignore.

handle_error(eargunused, _Level, {Function, NumArgs}) ->
    io_lib:format("~s(): ~p arguments unused", [Function, NumArgs]);

handle_error(efewargs, _Level, {Function, Cmd}) ->
    io_lib:format("~s(): Type ~s: too few arguments", [Function, Cmd]);

handle_error(eoutstr, _Level, {Function, Cmd}) ->
    io_lib:format("~s(): Type ~s: outside of string", [Function, Cmd]);

handle_error(eunknownfmt, _Level, {Function, Cmd}) ->
    io_lib:format("~s(): Type ~s: unknown format code", [Function, Cmd]);

handle_error(ecmdignored, _Level, {Function, Cmd, Op}) ->
    io_lib:format("~s(): Type ~s: '~s' ignored", [Function, Cmd, Op]);

handle_error(enoenoughin, _Level, {Function, Cmd, Size1, Size2}) ->
    io_lib:format("~s(): Type ~s: not enough input, need ~p, have ~p",
                  [Function, Cmd, Size1, Size2]);

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

exit(_Context, _Line, {_, Value}) when is_integer(Value)
                                orelse Value =:= undefined ->
    throw(die);

exit(Context, _Line, {_, Value}) ->
    ephp_context:set_output(Context, Value),
    throw(die).


-spec pack(context(), line(), [var_value()]) -> binary().

pack(Context, Line, [{_, Format}|Args]) ->
    do_pack(Context, Line, ephp_data:to_bin(Format),
            [ A || {_, A} <- Args ], <<>>).


-spec unpack(context(), line(), var_value(), var_value()) -> ephp_array().

unpack(Context, Line, {_, Format}, {_, Binary}) ->
    do_unpack(Context, Line,
              binary:split(ephp_data:to_bin(Format), <<"/">>, [global]),
              Binary, ephp_array:new(), <<>>).


%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

do_unpack(_Context, _Line, [], _Binary, Array, _PrevBin) ->
    Array;

do_unpack(Context, Line, [<<"a*", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    String = ephp_data:to_bin(Binary),
    NewArray = case Rest of
        <<>> -> ephp_array:store(1, String, Array);
        _ -> ephp_array:store(Rest, String, Array)
    end,
    do_unpack(Context, Line, Format, <<>>, NewArray, PrevBin);

do_unpack(Context, Line, [<<"a", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Rest0} = get_numbers(Rest, 1),
    String = ephp_data:to_bin(Binary),
    Index = case Rest0 of
        <<>> -> 1;
        _ -> Rest0
    end,
    case Size - byte_size(String) of
        N when N > 0 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"a">>, Size, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        N when N =:= 0 ->
            Array0 = ephp_array:store(Index, String, Array),
            PrevStr = <<PrevBin/binary, String/binary>>,
            do_unpack(Context, Line, Format, <<>>, Array0, PrevStr);
        N when N < 0 ->
            <<String1:Size/binary, RestStr/binary>> = String,
            Array0 = ephp_array:store(Index, String1, Array),
            PreStr = <<PrevBin/binary, String1/binary>>,
            do_unpack(Context, Line, Format, RestStr, Array0, PreStr)
    end;

do_unpack(Context, Line, [<<"A*", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    String = ephp_string:rtrim(ephp_data:to_bin(Binary), [32]),
    NewArray = case Rest of
        <<>> -> ephp_array:store(1, String, Array);
        _ -> ephp_array:store(Rest, String, Array)
    end,
    do_unpack(Context, Line, Format, <<>>, NewArray, PrevBin);

do_unpack(Context, Line, [<<"A", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Rest0} = get_numbers(Rest, 1),
    String = ephp_data:to_bin(Binary),
    Index = case Rest0 of
        <<>> -> 1;
        _ -> Rest0
    end,
    case Size - byte_size(String) of
        N when N > 0 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"A">>, Size, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        N when N =:= 0 ->
            SStr = ephp_string:rtrim(String, [32]),
            Array0 = ephp_array:store(Index, SStr, Array),
            PrevStr = <<PrevBin/binary, String/binary>>,
            do_unpack(Context, Line, Format, <<>>, Array0, PrevStr);
        N when N < 0 ->
            <<String1:Size/binary, RestStr/binary>> = String,
            SStr = ephp_string:rtrim(String1, [32]),
            Array0 = ephp_array:store(Index, SStr, Array),
            PreStr = <<PrevBin/binary, String1/binary>>,
            do_unpack(Context, Line, Format, RestStr, Array0, PreStr)
    end;

do_unpack(Context, Line, [<<"Z*", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    String = ephp_string:rtrim(ephp_data:to_bin(Binary), [0]),
    NewArray = case Rest of
        <<>> -> ephp_array:store(1, String, Array);
        _ -> ephp_array:store(Rest, String, Array)
    end,
    do_unpack(Context, Line, Format, <<>>, NewArray, PrevBin);

do_unpack(Context, Line, [<<"Z", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Rest0} = get_numbers(Rest, 1),
    String = ephp_data:to_bin(Binary),
    Index = case Rest0 of
        <<>> -> 1;
        _ -> Rest0
    end,
    case Size - byte_size(String) of
        N when N > 0 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"Z">>, Size, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        N when N =:= 0 ->
            ZStr = ephp_string:rtrim(String, [0]),
            Array0 = ephp_array:store(Index, ZStr, Array),
            PrevStr = <<PrevBin/binary, String/binary>>,
            do_unpack(Context, Line, Format, <<>>, Array0, PrevStr);
        N when N < 0 ->
            <<String1:Size/binary, RestStr/binary>> = String,
            ZStr = ephp_string:rtrim(String1, [0]),
            Array0 = ephp_array:store(Index, ZStr, Array),
            PreStr = <<PrevBin/binary, String1/binary>>,
            do_unpack(Context, Line, Format, RestStr, Array0, PreStr)
    end;

do_unpack(Context, Line, [<<"h*", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    String = ephp_string:ibin2hex(ephp_data:to_bin(Binary)),
    NewArray = case Rest of
        <<>> -> ephp_array:store(1, String, Array);
        _ -> ephp_array:store(Rest, String, Array)
    end,
    PreStr = <<PrevBin/binary, Binary/binary>>,
    do_unpack(Context, Line, Format, <<>>, NewArray, PreStr);

do_unpack(Context, Line, [<<"h", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Rest0} = get_numbers(Rest, 1),
    Str = ephp_data:to_bin(Binary),
    String = ephp_string:ibin2hex(Str),
    Index = case Rest0 of
        <<>> -> 1;
        _ -> Rest0
    end,
    case Size - byte_size(String) of
        N when N > 0 ->
            File = ephp_context:get_active_file(Context),
            TotalSize = ephp_data:ceiling(Size / 2),
            Data = {<<"unpack">>, <<"h">>, TotalSize, byte_size(Str)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        N when N =:= 0 ->
            Array0 = ephp_array:store(Index, String, Array),
            PrevStr = <<PrevBin/binary, Binary/binary>>,
            do_unpack(Context, Line, Format, <<>>, Array0, PrevStr);
        N when N < 0 ->
            TotalSize = ephp_data:ceiling(Size / 2),
            <<String1:Size/binary, _/binary>> = String,
            <<PrevStr:TotalSize/binary, RestStr/binary>> = Str,
            Array0 = ephp_array:store(Index, String1, Array),
            PStr = <<PrevBin/binary, PrevStr/binary>>,
            do_unpack(Context, Line, Format, RestStr, Array0, PStr)
    end;

do_unpack(Context, Line, [<<"H*", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    String = ephp_string:bin2hex(ephp_data:to_bin(Binary)),
    NewArray = case Rest of
        <<>> -> ephp_array:store(1, String, Array);
        _ -> ephp_array:store(Rest, String, Array)
    end,
    PrevStr = <<PrevBin/binary, Binary/binary>>,
    do_unpack(Context, Line, Format, <<>>, NewArray, PrevStr);

do_unpack(Context, Line, [<<"H", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Rest0} = get_numbers(Rest, 1),
    Str = ephp_data:to_bin(Binary),
    String = ephp_string:bin2hex(Str),
    Index = case Rest0 of
        <<>> -> 1;
        _ -> Rest0
    end,
    case Size - byte_size(String) of
        N when N > 0 ->
            File = ephp_context:get_active_file(Context),
            TotalSize = ephp_data:ceiling(Size / 2),
            Data = {<<"unpack">>, <<"H">>, TotalSize, byte_size(Str)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        N when N =:= 0 ->
            Array0 = ephp_array:store(Index, String, Array),
            PrevStr = <<PrevBin/binary, Binary/binary>>,
            do_unpack(Context, Line, Format, <<>>, Array0, PrevStr);
        N when N < 0 ->
            TotalSize = ephp_data:ceiling(Size / 2),
            <<String1:Size/binary, _/binary>> = String,
            <<PrevStr:TotalSize/binary, RestStr/binary>> = Str,
            Array0 = ephp_array:store(Index, String1, Array),
            PStr = <<PrevBin/binary, PrevStr/binary>>,
            do_unpack(Context, Line, Format, RestStr, Array0, PStr)
    end;

do_unpack(Context, Line, [<<"c*", Idx/binary>>|Format], Binary, Array,
          PrevBin) ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, <<>>, Prev) ->
            {Array0, Prev};
        (P, I, Array0, <<Char:8/integer-signed, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Char, Array0),
            PrevStr = <<Prev/binary, Char:8/integer-signed>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Char:8/integer-signed, String/binary>>, Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Char, Array0),
            PrevStr = <<Prev/binary, Char:8/integer-signed>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<"c", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, <<>>, 0, Prev) ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, <<>>, _N, _Prev) ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"c">>, 1, 0},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Char:8/integer-signed, String/binary>>, 1, Prev)
                when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Char, Array0),
            PrevStr = <<Prev/binary, Char:8/integer-signed>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Char:8/integer-signed, String/binary>>, N, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Char, Array0),
            PrevStr = <<Prev/binary, Char:8/integer-signed>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Char:8/integer-signed, String/binary>>, N, Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Char, Array0),
            PrevStr = <<Prev/binary, Char:8/integer-signed>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<"C*", Idx/binary>>|Format], Binary, Array,
          PrevBin) ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, <<>>, Prev) ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (P, I, Array0, <<Char:8/integer-unsigned, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Char, Array0),
            PrevStr = <<Prev/binary, Char:8/integer-unsigned>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Char:8/integer-unsigned, String/binary>>, Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Char, Array0),
            PrevStr = <<Prev/binary, Char:8/integer-unsigned>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    Process(Process, 1, Array, Binary, PrevBin);

do_unpack(Context, Line, [<<"C", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, <<>>, 0, Prev) ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, <<>>, _N, _Prev) ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"C">>, 1, 0},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Char:8/integer-unsigned, String/binary>>, 1, Prev)
                when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Char, Array0),
            PrevStr = <<Prev/binary, Char:8/integer-unsigned>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Char:8/integer-unsigned, String/binary>>, N, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Char, Array0),
            PrevStr = <<Prev/binary, Char:8/integer-unsigned>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Char:8/integer-unsigned, String/binary>>, N, Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Char, Array0),
            PrevStr = <<Prev/binary, Char:8/integer-unsigned>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<"s*", Idx/binary>>|Format], Binary, Array,
          PrevBin) ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 16 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:16/integer-signed-little, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-signed-little>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:16/integer-signed-little, String/binary>>, Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-signed-little>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<"s", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 16 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 16 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"s">>, 2, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:16/integer-signed-little, String/binary>>, 1,
         Prev) when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-signed-little>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:16/integer-signed-little, String/binary>>, N, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-signed-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:16/integer-signed-little, String/binary>>, N,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-signed-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<"n*", Idx/binary>>|Format], Binary, Array,
          PrevBin) ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 16 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:16/integer-unsigned-big, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-unsigned-big>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:16/integer-unsigned-big, String/binary>>, Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-unsigned-big>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<"n", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 16 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 16 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"n">>, 2, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:16/integer-unsigned-big, String/binary>>, 1,
         Prev) when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-unsigned-big>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:16/integer-unsigned-big, String/binary>>, N, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-unsigned-big>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:16/integer-unsigned-big, String/binary>>, N,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-unsigned-big>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<A:8, "*", Idx/binary>>|Format], Binary, Array,
          PrevBin) when A =:= $v orelse A =:= $S ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 16 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:16/integer-unsigned-little, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-unsigned-little>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:16/integer-unsigned-little, String/binary>>,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-unsigned-little>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<A:8, Rest/binary>>|Format], Binary, Array,
          PrevBin) when A =:= $v orelse A =:= $S ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 16 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 16 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<A:8>>, 2, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:16/integer-unsigned-little, String/binary>>, 1,
         Prev) when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-unsigned-little>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:16/integer-unsigned-little, String/binary>>, N,
         Prev) when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-unsigned-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:16/integer-unsigned-little, String/binary>>, N,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:16/integer-unsigned-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<A:8, "*", Idx/binary>>|Format], Binary, Array,
          PrevBin) when A =:= $I orelse A =:= $L orelse A =:= $V ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 32 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:32/integer-unsigned-little, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-unsigned-little>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:32/integer-unsigned-little, String/binary>>,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-unsigned-little>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<A:8, Rest/binary>>|Format], Binary, Array,
          PrevBin) when A =:= $I orelse A =:= $L orelse A =:= $V ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 32 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 32 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<A:8>>, 4, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:32/integer-unsigned-little, String/binary>>, 1,
         Prev) when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-unsigned-little>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:32/integer-unsigned-little, String/binary>>, N,
         Prev) when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-unsigned-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:32/integer-unsigned-little, String/binary>>, N,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-unsigned-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<A:8, "*", Idx/binary>>|Format], Binary, Array,
          PrevBin) when A =:= $i orelse A =:= $l ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 32 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:32/integer-signed-little, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-signed-little>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:32/integer-signed-little, String/binary>>,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-signed-little>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<A:8, Rest/binary>>|Format], Binary, Array,
          PrevBin) when A =:= $i orelse A =:= $l ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 32 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 32 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<A:8>>, 4, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:32/integer-signed-little, String/binary>>, 1,
         Prev) when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-signed-little>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:32/integer-signed-little, String/binary>>, N,
         Prev) when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-signed-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:32/integer-signed-little, String/binary>>, N,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-signed-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<"N*", Idx/binary>>|Format], Binary, Array,
          PrevBin) ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 32 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:32/integer-unsigned-big, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-unsigned-big>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:32/integer-unsigned-big, String/binary>>,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-unsigned-big>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<"N", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 32 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 32 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"N">>, 4, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:32/integer-unsigned-big, String/binary>>, 1,
         Prev) when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-unsigned-big>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:32/integer-unsigned-big, String/binary>>, N,
         Prev) when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-unsigned-big>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:32/integer-unsigned-big, String/binary>>, N,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/integer-unsigned-big>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<A:8, "*", Idx/binary>>|Format], Binary, Array,
          PrevBin) when A =:= $Q orelse A =:= $P ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 64 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:64/integer-unsigned-little, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-unsigned-little>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:64/integer-unsigned-little, String/binary>>,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-unsigned-little>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<A:8, Rest/binary>>|Format], Binary, Array,
          PrevBin) when A =:= $Q orelse A =:= $P ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 64 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 64 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<A:8>>, 8, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:64/integer-unsigned-little, String/binary>>, 1,
         Prev) when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-unsigned-little>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:64/integer-unsigned-little, String/binary>>, N,
         Prev) when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-unsigned-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:64/integer-unsigned-little, String/binary>>, N,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-unsigned-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<"q*", Idx/binary>>|Format], Binary, Array,
          PrevBin) ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 64 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:64/integer-signed-little, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-signed-little>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:64/integer-signed-little, String/binary>>,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-signed-little>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<"q", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 64 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 64 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"q">>, 8, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:64/integer-signed-little, String/binary>>, 1,
         Prev) when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-signed-little>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:64/integer-signed-little, String/binary>>, N,
         Prev) when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-signed-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:64/integer-signed-little, String/binary>>, N,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-signed-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<"J*", Idx/binary>>|Format], Binary, Array,
          PrevBin) ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 32 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:64/integer-unsigned-big, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-unsigned-big>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:64/integer-unsigned-big, String/binary>>,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-unsigned-big>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<"J", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 64 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 64 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"J">>, 8, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:64/integer-unsigned-big, String/binary>>, 1,
         Prev) when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-unsigned-big>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:64/integer-unsigned-big, String/binary>>, N,
         Prev) when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-unsigned-big>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:64/integer-unsigned-big, String/binary>>, N,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/integer-unsigned-big>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<"f*", Idx/binary>>|Format], Binary, Array,
          PrevBin) ->
    %% FIXME when R16 is not supported anymore it could be rewrot, PrevBine:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 32 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:32/float-little, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/float-little>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:32/float-little, String/binary>>,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/float-little>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<"f", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 32 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 32 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"f">>, 4, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:32/float-little, String/binary>>, 1, Prev)
                when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/float-little>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:32/float-little, String/binary>>, N, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/float-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:32/float-little, String/binary>>, N, Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:32/float-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<"d*", Idx/binary>>|Format], Binary, Array,
          PrevBin) ->
    %% FIXME when R16 is not supported anymore it could be rewrote:
    Process = fun
        (_P, _I, Array0, String, Prev) when bit_size(String) < 64 ->
            {Array0, Prev};
        (P, I, Array0, <<Bin:64/float-little, String/binary>>, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/float-little>>,
            P(P, I+1, Array1, String, PrevStr);
        (P, I, Array0, <<Bin:64/float-little, String/binary>>,
         Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/float-little>>,
            P(P, I+1, Array1, String, PrevStr)
    end,
    {Array0, PrevBin0} = Process(Process, 1, Array, Binary, PrevBin),
    do_unpack(Context, Line, Format, <<>>, Array0, PrevBin0);

do_unpack(Context, Line, [<<"d", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, Idx} = get_numbers(Rest, 1),
    Process = fun
        (_P, _I, Array0, String, 0, Prev) when bit_size(String) < 64 ->
            do_unpack(Context, Line, Format, <<>>, Array0, Prev);
        (_P, _I, _Array0, String, _N, _Prev) when bit_size(String) < 64 ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"d">>, 8, byte_size(String)},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        (_P, 1, Array0, <<Bin:64/float-little, String/binary>>, 1, Prev)
                when Idx =/= <<>> ->
            Array1 = ephp_array:store(Idx, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/float-little>>,
            do_unpack(Context, Line, Format, String, Array1, PrevStr);
        (_P, _I, Array0, String, 0, Prev) ->
            do_unpack(Context, Line, Format, String, Array0, Prev);
        (P, I, Array0, <<Bin:64/float-little, String/binary>>, N, Prev)
                when Idx =:= <<>> ->
            Array1 = ephp_array:store(I, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/float-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr);
        (P, I, Array0, <<Bin:64/float-little, String/binary>>, N, Prev) ->
            Index = <<Idx/binary, (integer_to_binary(I))/binary>>,
            Array1 = ephp_array:store(Index, Bin, Array0),
            PrevStr = <<Prev/binary, Bin:64/float-little>>,
            P(P, I+1, Array1, String, N-1, PrevStr)
    end,
    Process(Process, 1, Array, Binary, Size, PrevBin);

do_unpack(Context, Line, [<<A:8, "*", _/binary>>|Format], _Binary, Array,
          PrevBin) when A =:= $x orelse A =:= $@ ->
    do_unpack(Context, Line, Format, <<>>, Array, PrevBin);

do_unpack(Context, Line, [<<A:8, Rest/binary>>|Format], Binary, Array,
          PrevBin) when A =:= $x orelse A =:= $@ ->
    {Size, _} = get_numbers(Rest, 1),
    case byte_size(Binary) of
        N when N =:= Size ->
            do_unpack(Context, Line, Format, <<>>, Array, Binary);
        N when N < Size andalso A =:= $x ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"x">>, 1, 0},
            ephp_error:handle_error(Context, {error, enoenoughin, Line, File,
                                              ?E_WARNING, Data}),
            false;
        N when N < Size andalso A =:= $@ ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"unpack">>, <<"@">>},
            ephp_error:handle_error(Context, {error, eoutstr, Line, File,
                                              ?E_WARNING, Data}),
            do_unpack(Context, Line, Format, Binary, Array, PrevBin);
        N when N > Size ->
            <<PreStr:Size/binary, RestBin/binary>> = Binary,
            do_unpack(Context, Line, Format, RestBin, Array, PreStr)
    end;

do_unpack(Context, Line, [<<"X", Rest/binary>>|Format], Binary, Array,
          PrevBin) ->
    {Size, _} = get_numbers(Rest, 1),
    Process = fun
        (_P, 0, Bin, Prev) ->
            {Bin, Prev};
        (_P, _I, Bin, <<>>) ->
            {Bin, <<>>};
        (_P, _I, Bin, <<A:1/binary>>) ->
            {<<A/binary, Bin/binary>>, <<>>};
        (P, I, Bin, Prev) ->
            PSize = byte_size(Prev) - 1,
            <<PBin:PSize/binary, X:1/binary>> = Prev,
            P(P, I-1, <<X/binary, Bin/binary>>, PBin)
    end,
    {Binary0, PrevBin0} = Process(Process, Size, Binary, PrevBin),
    do_unpack(Context, Line, Format, Binary0, Array, PrevBin0).


do_pack(_Context, _Line, <<>>, [], Binary) ->
    Binary;

do_pack(Context, Line, <<>>, Args, Binary) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"pack">>, length(Args)},
    ephp_error:handle_error(Context, {error, eargunused, Line, File,
                                      ?E_WARNING, Data}),
    Binary;

do_pack(Context, Line, <<A:8, "*", Rest/binary>>, [Arg|Args], Binary)
        when A =:= $A orelse A =:= $a ->
    String = ephp_data:to_bin(Arg),
    do_pack(Context, Line, Rest, Args, <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"a", Rest/binary>>, [Arg|Args], Binary) ->
    {Size, Rest0} = get_numbers(Rest, 0),
    String = ephp_data:to_bin(Arg),
    String0 = case Size - byte_size(String) of
        N when N =:= 0 ->
            <<Binary/binary, String/binary>>;
        N when N < 0 ->
            <<Binary/binary, String:Size/binary>>;
        N when N > 0 ->
            Nulls = ephp_string:repeat(N, 0),
            <<Binary/binary, String/binary, Nulls/binary>>
    end,
    do_pack(Context, Line, Rest0, Args, String0);

do_pack(Context, Line, <<"A", Rest/binary>>, [Arg|Args], Binary) ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    String = ephp_data:to_bin(Arg),
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
    {Size0, Rest0} = get_numbers(Rest, 1),
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
    {Size0, Rest0} = get_numbers(Rest, 1),
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

do_pack(Context, Line, <<C:8, "*", Rest/binary>>, Args, Binary)
        when C =:= $c orelse C =:= $C ->
    String = lists:foldl(fun
        (Arg, S) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            <<S/binary, A:8>>;
        (_, S) ->
            <<S/binary, 0:8>>
    end, <<>>, Args),
    do_pack(Context, Line, Rest, [], <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<C:8, Rest/binary>>, Args, Binary)
        when C =:= $c orelse C =:= $C ->
    {Size0, Rest0} = get_numbers(Rest, 1),
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
            Data = {<<"pack">>, <<C:8>>},
            ephp_error:handle_error(Context, {error, efewargs, Line, File,
                                              ?E_WARNING, Data}),
            false
    end;

do_pack(Context, Line, <<C:8, "*", Rest/binary>>, Args, Binary)
        when C =:= $s orelse C =:= $S orelse C =:= $v ->
    String = lists:foldl(fun
        (Arg, S) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            <<S/binary, A:16/integer-little>>;
        (_, S) ->
            <<S/binary, 0:16>>
    end, <<>>, Args),
    do_pack(Context, Line, Rest, [], <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<C:8, Rest/binary>>, Args, Binary)
        when C =:= $s orelse C =:= $S orelse C =:= $v ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    {NSize, NArgs, String} = lists:foldl(fun
        (Arg, {0, AA, Str}) ->
            {0, AA ++ [Arg], Str};
        (Arg, {Si, AA, Str}) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            {Si-1, AA, <<Str/binary, A:16/integer-little>>};
        (_, {Si, AA, Str}) ->
            {Si-1, AA, <<Str/binary, 0:16>>}
    end, {Size0, [], <<>>}, Args),
    case NSize of
        0 ->
            do_pack(Context, Line, Rest0, NArgs,
                    <<Binary/binary, String/binary>>);
        _ ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"pack">>, <<C:8>>},
            ephp_error:handle_error(Context, {error, efewargs, Line, File,
                                              ?E_WARNING, Data}),
            false
    end;

do_pack(Context, Line, <<"n*", Rest/binary>>, Args, Binary) ->
    String = lists:foldl(fun
        (Arg, S) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            <<S/binary, A:16/integer-big>>;
        (_, S) ->
            <<S/binary, 0:16>>
    end, <<>>, Args),
    do_pack(Context, Line, Rest, [], <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"n", Rest/binary>>, Args, Binary) ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    {NSize, NArgs, String} = lists:foldl(fun
        (Arg, {0, AA, Str}) ->
            {0, AA ++ [Arg], Str};
        (Arg, {Si, AA, Str}) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            {Si-1, AA, <<Str/binary, A:16/integer-big>>};
        (_, {Si, AA, Str}) ->
            {Si-1, AA, <<Str/binary, 0:16>>}
    end, {Size0, [], <<>>}, Args),
    case NSize of
        0 ->
            do_pack(Context, Line, Rest0, NArgs,
                    <<Binary/binary, String/binary>>);
        _ ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"pack">>, <<"n">>},
            ephp_error:handle_error(Context, {error, efewargs, Line, File,
                                              ?E_WARNING, Data}),
            false
    end;

do_pack(Context, Line, <<C:8, "*", Rest/binary>>, Args, Binary)
        when C =:= $i orelse C =:= $I orelse C =:= $l orelse
             C =:= $L orelse C =:= $V ->
    String = lists:foldl(fun
        (Arg, S) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            <<S/binary, A:32/integer-little>>;
        (_, S) ->
            <<S/binary, 0:32>>
    end, <<>>, Args),
    do_pack(Context, Line, Rest, [], <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<C:8, Rest/binary>>, Args, Binary)
        when C =:= $i orelse C =:= $I orelse C =:= $l orelse
             C =:= $L orelse C =:= $V ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    {NSize, NArgs, String} = lists:foldl(fun
        (Arg, {0, AA, Str}) ->
            {0, AA ++ [Arg], Str};
        (Arg, {Si, AA, Str}) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            {Si-1, AA, <<Str/binary, A:32/integer-little>>};
        (_, {Si, AA, Str}) ->
            {Si-1, AA, <<Str/binary, 0:32>>}
    end, {Size0, [], <<>>}, Args),
    case NSize of
        0 ->
            do_pack(Context, Line, Rest0, NArgs,
                    <<Binary/binary, String/binary>>);
        _ ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"pack">>, <<C:8>>},
            ephp_error:handle_error(Context, {error, efewargs, Line, File,
                                              ?E_WARNING, Data}),
            false
    end;

do_pack(Context, Line, <<"N*", Rest/binary>>, Args, Binary) ->
    String = lists:foldl(fun
        (Arg, S) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            <<S/binary, A:32/integer-big>>;
        (_, S) ->
            <<S/binary, 0:32>>
    end, <<>>, Args),
    do_pack(Context, Line, Rest, [], <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"N", Rest/binary>>, Args, Binary) ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    {NSize, NArgs, String} = lists:foldl(fun
        (Arg, {0, AA, Str}) ->
            {0, AA ++ [Arg], Str};
        (Arg, {Si, AA, Str}) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            {Si-1, AA, <<Str/binary, A:32/integer-big>>};
        (_, {Si, AA, Str}) ->
            {Si-1, AA, <<Str/binary, 0:32>>}
    end, {Size0, [], <<>>}, Args),
    case NSize of
        0 ->
            do_pack(Context, Line, Rest0, NArgs,
                    <<Binary/binary, String/binary>>);
        _ ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"pack">>, <<"N">>},
            ephp_error:handle_error(Context, {error, efewargs, Line, File,
                                              ?E_WARNING, Data}),
            false
    end;

do_pack(Context, Line, <<C:8, "*", Rest/binary>>, Args, Binary)
        when C =:= $q orelse C =:= $Q orelse C =:= $P->
    String = lists:foldl(fun
        (Arg, S) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            <<S/binary, A:64/integer-little>>;
        (_, S) ->
            <<S/binary, 0:64>>
    end, <<>>, Args),
    do_pack(Context, Line, Rest, [], <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<C:8, Rest/binary>>, Args, Binary)
        when C =:= $q orelse C =:= $Q orelse C =:= $P ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    {NSize, NArgs, String} = lists:foldl(fun
        (Arg, {0, AA, Str}) ->
            {0, AA ++ [Arg], Str};
        (Arg, {Si, AA, Str}) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            {Si-1, AA, <<Str/binary, A:64/integer-little>>};
        (_, {Si, AA, Str}) ->
            {Si-1, AA, <<Str/binary, 0:64>>}
    end, {Size0, [], <<>>}, Args),
    case NSize of
        0 ->
            do_pack(Context, Line, Rest0, NArgs,
                    <<Binary/binary, String/binary>>);
        _ ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"pack">>, <<C:8>>},
            ephp_error:handle_error(Context, {error, efewargs, Line, File,
                                              ?E_WARNING, Data}),
            false
    end;

do_pack(Context, Line, <<"J*", Rest/binary>>, Args, Binary) ->
    String = lists:foldl(fun
        (Arg, S) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            <<S/binary, A:64/integer-big>>;
        (_, S) ->
            <<S/binary, 0:64>>
    end, <<>>, Args),
    do_pack(Context, Line, Rest, [], <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"J", Rest/binary>>, Args, Binary) ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    {NSize, NArgs, String} = lists:foldl(fun
        (Arg, {0, AA, Str}) ->
            {0, AA ++ [Arg], Str};
        (Arg, {Si, AA, Str}) when is_number(Arg) ->
            A = ephp_data:flooring(Arg),
            {Si-1, AA, <<Str/binary, A:64/integer-big>>};
        (_, {Si, AA, Str}) ->
            {Si-1, AA, <<Str/binary, 0:64>>}
    end, {Size0, [], <<>>}, Args),
    case NSize of
        0 ->
            do_pack(Context, Line, Rest0, NArgs,
                    <<Binary/binary, String/binary>>);
        _ ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"pack">>, <<"J">>},
            ephp_error:handle_error(Context, {error, efewargs, Line, File,
                                              ?E_WARNING, Data}),
            false
    end;

do_pack(Context, Line, <<"f*", Rest/binary>>, Args, Binary) ->
    String = lists:foldl(fun
        (Arg, S) when is_number(Arg) ->
            <<S/binary, Arg:32/float-little>>;
        (_, S) ->
            <<S/binary, 0:32>>
    end, <<>>, Args),
    do_pack(Context, Line, Rest, [], <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"f", Rest/binary>>, Args, Binary) ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    {NSize, NArgs, String} = lists:foldl(fun
        (Arg, {0, AA, Str}) ->
            {0, AA ++ [Arg], Str};
        (Arg, {Si, AA, Str}) when is_number(Arg) ->
            Number = float(Arg),
            {Si-1, AA, <<Str/binary, Number:32/float-little>>};
        (_, {Si, AA, Str}) ->
            {Si-1, AA, <<Str/binary, 0:32>>}
    end, {Size0, [], <<>>}, Args),
    case NSize of
        0 ->
            do_pack(Context, Line, Rest0, NArgs,
                    <<Binary/binary, String/binary>>);
        _ ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"pack">>, <<"f">>},
            ephp_error:handle_error(Context, {error, efewargs, Line, File,
                                              ?E_WARNING, Data}),
            false
    end;

do_pack(Context, Line, <<"d*", Rest/binary>>, Args, Binary) ->
    String = lists:foldl(fun
        (Arg, S) when is_number(Arg) ->
            <<S/binary, Arg:64/float-little>>;
        (_, S) ->
            <<S/binary, 0:64>>
    end, <<>>, Args),
    do_pack(Context, Line, Rest, [], <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"d", Rest/binary>>, Args, Binary) ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    {NSize, NArgs, String} = lists:foldl(fun
        (Arg, {0, AA, Str}) ->
            {0, AA ++ [Arg], Str};
        (Arg, {Si, AA, Str}) when is_number(Arg) ->
            Number = float(Arg),
            {Si-1, AA, <<Str/binary, Number:64/float-little>>};
        (_, {Si, AA, Str}) ->
            {Si-1, AA, <<Str/binary, 0:64>>}
    end, {Size0, [], <<>>}, Args),
    case NSize of
        0 ->
            do_pack(Context, Line, Rest0, NArgs,
                    <<Binary/binary, String/binary>>);
        _ ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"pack">>, <<"d">>},
            ephp_error:handle_error(Context, {error, efewargs, Line, File,
                                              ?E_WARNING, Data}),
            false
    end;

do_pack(Context, Line, <<"X*", Rest/binary>>, Args, <<>>) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"pack">>, <<"X">>},
    ephp_error:handle_error(Context, {error, eoutstr, Line, File,
                                      ?E_WARNING, Data}),
    do_pack(Context, Line, Rest, Args, <<>>);

do_pack(Context, Line, <<"X", Rest/binary>>, Args, <<>>) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"pack">>, <<"X">>},
    ephp_error:handle_error(Context, {error, eoutstr, Line, File,
                                      ?E_WARNING, Data}),
    do_pack(Context, Line, Rest, Args, <<>>);

do_pack(Context, Line, <<"X*", Rest/binary>>, Args, Binary) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"pack">>, <<"X">>, <<"*">>},
    ephp_error:handle_error(Context, {error, ecmdignored, Line, File,
                                      ?E_WARNING, Data}),
    do_pack(Context, Line, <<"X", Rest/binary>>, Args, Binary);

do_pack(Context, Line, <<"X", Rest/binary>>, Args, Binary) ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    Result = lists:foldl(fun
        (_, <<>>) ->
            File = ephp_context:get_active_file(Context),
            Data = {<<"pack">>, <<"X">>},
            ephp_error:handle_error(Context, {error, eoutstr, Line, File,
                                              ?E_WARNING, Data}),
            undefined;
        (_, undefined) ->
            undefined;
        (_, <<_:1/binary>>) ->
            <<>>;
        (_, B) ->
            binary_part(B, {0, byte_size(Binary) - 1})
    end, Binary, lists:seq(1, Size0)),
    Binary0 = if
        Result =:= undefined -> <<>>;
        true -> Result
    end,
    do_pack(Context, Line, Rest0, Args, Binary0);

do_pack(Context, Line, <<"x*", Rest/binary>>, Args, Binary) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"pack">>, <<"x">>, <<"*">>},
    ephp_error:handle_error(Context, {error, ecmdignored, Line, File,
                                      ?E_WARNING, Data}),
    do_pack(Context, Line, Rest, Args, <<Binary/binary, 0:8>>);

do_pack(Context, Line, <<"x", Rest/binary>>, Args, Binary) ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    String = ephp_string:repeat(Size0, 0),
    do_pack(Context, Line, Rest0, Args, <<Binary/binary, String/binary>>);

do_pack(Context, Line, <<"Z*", Rest/binary>>, [Arg|Args], Binary) ->
    String = ephp_data:to_bin(Arg),
    do_pack(Context, Line, Rest, Args, <<Binary/binary, String/binary, 0:8>>);

do_pack(Context, Line, <<"Z", Rest/binary>>, [Arg|Args], Binary) ->
    {Size0, Rest0} = get_numbers(Rest, 1),
    String = ephp_data:to_bin(Arg),
    Binary0 = case Size0 - byte_size(String) of
        _ when Size0 =:= 0 ->
            Binary;
        _ when Size0 =:= 1 ->
            <<Binary/binary, 0:8>>;
        N when N =< 0 ->
            Size1 = Size0 - 1,
            <<Binary/binary, String:Size1/binary, 0:8>>;
        N when N > 0 ->
            Nulls = ephp_string:repeat(N, 0),
            <<Binary/binary, String/binary, Nulls/binary>>
    end,
    do_pack(Context, Line, Rest0, Args, Binary0);

do_pack(Context, Line, <<"@", Rest/binary>>, Args, Binary) ->
    {Pos, Rest0} = get_numbers(Rest, 1),
    Size = byte_size(Binary),
    Binary0 = if
        Size > Pos ->
            binary_part(Binary, {0, Pos});
        Size =:= Pos ->
            Binary;
        true ->
            Pad = Pos - Size,
            Nulls = ephp_string:repeat(Pad, 0),
            <<Binary/binary, Nulls/binary>>
    end,
    do_pack(Context, Line, Rest0, Args, Binary0);

do_pack(Context, Line, <<C:1/binary, _/binary>>, _Args, _Binary) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"pack">>, C},
    ephp_error:handle_error(Context, {error, eunknownfmt, Line, File,
                                      ?E_WARNING, Data}),
    <<>>.


-spec get_numbers(binary(), integer()) -> {non_neg_integer(), binary()}.
%% @doc retrieve numbers from binary while it's possible.
get_numbers(Binary, Default) ->
    get_numbers(Binary, <<>>, Default).

-spec get_numbers(binary(), binary(), integer()) ->
      {non_neg_integer(), binary()}.
%% @private
get_numbers(<<>>, <<>>, Default) ->
    {Default, <<>>};
get_numbers(<<>>, Num, _Default) ->
    {binary_to_integer(Num), <<>>};
get_numbers(<<A:8,Rest/binary>>, Num, Default) when A >= $0 andalso A =< $9 ->
    get_numbers(Rest, <<Num/binary, A:8>>, Default);
get_numbers(Rest, <<>>, Default) ->
    {Default, Rest};
get_numbers(Rest, Num, _Default) ->
    {binary_to_integer(Num), Rest}.

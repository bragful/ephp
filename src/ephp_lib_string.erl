-module(ephp_lib_string).

-author('manuel@altenwald.com').

-behaviour(ephp_lib).

-export([init_func/0, init_config/0, init_const/0, handle_error/3, bin2hex/3, hex2bin/3,
         strlen/3, ord/3, chr/3, implode/3, implode/4, explode/4, explode/5, print/3, printf/3,
         sprintf/3, vprintf/4, vsprintf/4, str_replace/5, str_replace/6, strtolower/3,
         strtoupper/3, ucfirst/3, lcfirst/3, ucwords/4, str_shuffle/3, str_split/3, str_split/4,
         strpos/5, strrpos/5, strrev/3, rtrim/4, ltrim/4, trim/4, md5/4, substr/5, str_pad/6,
         str_repeat/4, count_chars/4, nl2br/4]).

-include("ephp.hrl").
-include("ephp_array.hrl").

-define(STR_PAD_LEFT, 0).
-define(STR_PAD_RIGHT, 1).
-define(STR_PAD_BOTH, 2).

-spec init_func() -> ephp_lib:php_function_results().
init_func() ->
    [{strlen, [{args, [string]}]}, {ord, [{args, [string]}]}, {chr, [{args, [str_or_int]}]},
     %% TODO: automatic validation for implode is not possible because it has
     %%       an overloading not supported by default by PHP
     implode,
     {implode, [{alias, <<"join">>}]},
     {explode, [{args, [string, string, {integer, ?PHP_INT_MAX}]}]},
     %% FIXME: split is deprecated, should be removed?
     {explode, [{args, [string, string, {integer, ?PHP_INT_MAX}]}, {alias, <<"split">>}]},
     {print, [pack_args]}, {print, [pack_args, {alias, <<"echo">>}]}, {printf, [pack_args]},
     {sprintf, [pack_args]}, vprintf, vsprintf, str_replace, strtolower, strtoupper, ucfirst,
     lcfirst, str_shuffle,
     {ucwords, [{args, {1, 2, undefined, [string, {string, <<32, 9, 10, 13, 11>>}]}}]},
     str_split,
     {strpos,
      [{args,
        {2,
         3,
         undefined,
         [string,
          {str_or_int, undefined, {eisnot, [<<"needle">>, <<"a string or an integer">>]}},
          {integer, 0}]}}]},
     {strrpos,
      [{args,
        {2,
         3,
         undefined,
         [string,
          {str_or_int, undefined, {eisnot, [<<"needle">>, <<"a string or an integer">>]}},
          {integer, 0}]}}]},
     {strrev, [{args, {1, 1, undefined, [string]}}]},
     {ltrim, [{args, {1, 2, undefined, [string, {string, <<32, 9, 10, 13, 0, 11>>}]}}]},
     {rtrim, [{args, {1, 2, undefined, [string, {string, <<32, 9, 10, 13, 0, 11>>}]}}]},
     {rtrim,
      [{args, {1, 2, undefined, [string, {string, <<32, 9, 10, 13, 0, 11>>}]}},
       {alias, <<"chop">>}]},
     {trim, [{args, {1, 2, undefined, [string, {string, <<32, 9, 10, 13, 0, 11>>}]}}]},
     {md5, [{args, {1, 2, undefined, [string, {boolean, false}]}}]},
     {bin2hex, [{args, {1, 1, undefined, [string]}}]},
     {hex2bin, [{args, {1, 1, undefined, [string]}}]},
     {substr, [{args, {2, 3, undefined, [string, integer, {integer, eol}]}}]},
     {str_pad,
      [{args,
        {2, 4, undefined, [string, integer, {string, <<" ">>}, {integer, ?STR_PAD_RIGHT}]}}]},
     {str_repeat, [{args, [string, integer]}]},
     {count_chars, [{args, {1, 2, undefined, [string, {integer, 0}]}}]},
     {nl2br, [{args, {1, 2, undefined, [string, {boolean, true}]}}]}].

-spec init_config() -> ephp_lib:php_config_results().
%% @private
init_config() ->
    [].

-spec init_const() -> ephp_lib:php_const_results().
%% @private
init_const() ->
    [{<<"STR_PAD_LEFT">>, ?STR_PAD_LEFT},
     {<<"STR_PAD_RIGHT">>, ?STR_PAD_RIGHT},
     {<<"STR_PAD_BOTH">>, ?STR_PAD_BOTH}].

-spec handle_error(ephp_error:error_type(), ephp_error:error_level(), Args :: term()) ->
                      string() | ignore.
%% @doc handle error messages.
handle_error(ehexeven, _Level, {Function}) ->
    io_lib:format("~s(): Hexadecimal input string must have an even length", [Function]);
handle_error(e2posint, _Level, {Function}) ->
    io_lib:format("~s(): Second argument has to be greater than or equal to 0", [Function]);
handle_error(_Type, _Level, _Data) ->
    ignore.

-spec strlen(ephp:context_id(), line(), String :: var_value()) -> integer().
%% @doc retrieve the length of the string.
strlen(_Context, _Line, {_, String}) when is_binary(String) ->
    byte_size(String).

-spec ord(ephp:context_id(), line(), String :: var_value()) -> integer().
%% @doc obtain the number of the character passed as a param.
ord(_Context, _Line, {_, <<I:8/integer, _/binary>>}) ->
    I.

-spec chr(ephp:context_id(), line(), Integer :: var_value()) -> binary().
%% @doc obtain the character giving the number as a param.
chr(_Context, _Line, {_, C}) when is_integer(C) ->
    <<C:8/integer>>;
chr(_Context, _Line, _Var) ->
    undefined.

-spec implode(ephp:context_id(), line(), Glue :: var_value(), Pieces :: var_value()) ->
                 binary().
%% @doc join the array with the glue passed as a param.
implode(Context, Line, {_, Glue} = VarGlue, _Pieces) when ?IS_ARRAY(Glue) ->
    File = ephp_context:get_active_file(Context),
    Error = {error, earrayconv, Line, File, ?E_NOTICE, {<<"string">>}},
    ephp_error:handle_error(Context, Error),
    implode(Context, Line, {undefined, <<"Array">>}, VarGlue);
implode(Context, Line, {_, RawGlue}, {_, Pieces}) when ?IS_ARRAY(Pieces) ->
    Glue = ephp_data:to_bin(Context, Line, RawGlue),
    ListOfPieces =
        ephp_array:fold(fun(_Key, Piece, SetOfPieces) ->
                           SetOfPieces ++ [ephp_data:to_bin(Context, Line, Piece)]
                        end,
                        [],
                        Pieces),
    case ListOfPieces of
        [] ->
            <<>>;
        [H | T] ->
            <<H/binary, << <<Glue/binary, X/binary>> || X <- T >>/binary>>
    end.

-spec implode(ephp:context_id(), line(), Pieces :: var_value()) -> binary().
%% @doc join the array passed as a param.
implode(Context, Line, Pieces) ->
    implode(Context, Line, {undefined, <<>>}, Pieces).

-spec explode(ephp:context_id(),
              line(),
              Delimiter :: var_value(),
              String :: var_value()) ->
                 ephp_array:ephp_array().
%% @doc split the string in pieces in an array.
explode(_Context, _Line, {_, Delimiter}, {_, String}) ->
    Pieces = binary:split(String, Delimiter, [global]),
    lists:foldl(fun(Piece, Explode) -> ephp_array:store(auto, Piece, Explode) end,
                ephp_array:new(),
                Pieces).

-spec explode(ephp:context_id(),
              line(),
              Delimiter :: var_value(),
              String :: var_value(),
              Limit :: var_value()) ->
                 ephp_array:ephp_array().
%% @doc split the string in pieces in an array with a limit.
explode(Context, Line, _Delimiter, _String, {_, Limit}) when not is_integer(Limit) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"explode">>, 3, <<"long">>, ephp_data:gettype(Limit)},
    ephp_error:handle_error(Context, {error, ewrongarg, Line, File, ?E_WARNING, Data}),
    undefined;
explode(_Context, _Line, _Delimiter, {_, String}, {_, 0}) ->
    String;
explode(_Context, _Line, {_, Delimiter}, {_, String}, {_, Limit}) ->
    split_limit(Delimiter, String, ephp_array:new(), Limit - 1, 0).

-spec printf(ephp:context_id(), line(), [var_value()]) -> pos_integer().
%% @doc print using a format.
printf(Context, Line, Values) when length(Values) < 2 ->
    File = ephp_context:get_active_file(Context),
    Error = {error, efewargs, Line, File, ?E_WARNING, {<<"printf">>}},
    ephp_error:handle_error(Context, Error),
    undefined;
printf(Context, _Line, [{_, Format} | Values]) ->
    Text = print_format(Format, Values),
    ephp_context:set_output(Context, Text),
    byte_size(Text).

-spec sprintf(ephp:context_id(), line(), [var_value()]) -> binary().
%% @doc generate a string using a format.
sprintf(Context, Line, Values) when length(Values) < 2 ->
    File = ephp_context:get_active_file(Context),
    Error = {error, efewargs, Line, File, ?E_WARNING, {<<"sprintf">>}},
    ephp_error:handle_error(Context, Error),
    undefined;
sprintf(_Context, _Line, [{_, Format} | Values]) ->
    print_format(Format, Values).

-spec vprintf(ephp:context_id(), line(), Format :: var_value(), Values :: var_value()) ->
                 pos_integer().
vprintf(Context, _Line, {_, Format}, {_, Values}) when ?IS_ARRAY(Values) ->
    Text = print_format(Format, ephp_array:to_list(Values)),
    ephp_context:set_output(Context, Text),
    byte_size(Text).

-spec vsprintf(ephp:context_id(), line(), Format :: var_value(), Values :: var_value()) ->
                  binary().
vsprintf(_Context, _Line, {_, Format}, {_, Values}) when ?IS_ARRAY(Values) ->
    print_format(Format, ephp_array:to_list(Values)).

-spec str_replace(ephp:context_id(),
                  line(),
                  Search :: var_value(),
                  Replace :: var_value(),
                  Subject :: var_value()) ->
                     binary().
str_replace(_Context, _Line, {_, Search}, {_, Replace}, {_, Subject})
    when ?IS_ARRAY(Search) andalso is_binary(Replace) ->
    ephp_array:fold(fun(_, V, Sub) -> binary:replace(Sub, V, Replace, [global]) end,
                    Subject,
                    Search);
str_replace(_Context, _Line, {_, Search}, {_, Replace}, {_, Subject})
    when ?IS_ARRAY(Search) andalso ?IS_ARRAY(Replace) ->
    A = ephp_array:to_list(Search),
    B = ephp_array:to_list(Replace),
    Lists = lists:zipwith(fun({_, V1}, {_, V2}) -> {V1, V2} end, A, B),
    lists:foldl(fun({Source, Target}, Sub) -> binary:replace(Sub, Source, Target, [global])
                end,
                Subject,
                Lists);
str_replace(_Context, _Line, {_, Search}, {_, Replace}, {_, Subject}) ->
    binary:replace(Subject, Search, Replace, [global]).

-spec str_replace(ephp:context_id(),
                  line(),
                  Search :: var_value(),
                  Replace :: var_value(),
                  Subject :: var_value(),
                  Count :: var_value()) ->
                     binary().
str_replace(Context, _Line, {_, Search}, {_, Replace}, {_, Subject}, {Count, _}) ->
    ephp_context:set(Context, Count, length(binary:matches(Subject, Search))),
    binary:replace(Subject, Search, Replace, [global]).

-spec strtolower(ephp:context_id(), line(), Text :: var_value()) -> binary().
strtolower(Context, Line, {_, Text}) ->
    ephp_string:to_lower(
        ephp_data:to_bin(Context, Line, Text)).

-spec strtoupper(ephp:context_id(), line(), Text :: var_value()) -> binary().
strtoupper(Context, Line, {_, Text}) ->
    ephp_string:to_upper(
        ephp_data:to_bin(Context, Line, Text)).

-spec ucfirst(ephp:context_id(), line(), Text :: var_value()) -> binary().
ucfirst(Context, Line, {_, Text}) ->
    case ephp_data:to_bin(Context, Line, Text) of
        <<>> ->
            <<>>;
        <<First/utf8, Tail/binary>> ->
            <<(ephp_string:to_upper(First))/utf8, Tail/binary>>
    end.

-spec lcfirst(ephp:context_id(), line(), Text :: var_value()) -> binary().
lcfirst(Context, Line, {_, Text}) ->
    case ephp_data:to_bin(Context, Line, Text) of
        <<>> ->
            <<>>;
        <<First/utf8, Tail/binary>> ->
            <<(ephp_string:to_lower(First))/utf8, Tail/binary>>
    end.

-spec str_shuffle(ephp:context_id(), line(), var_value()) -> binary().
str_shuffle(Context, Line, {_, Text}) ->
    case ephp_data:to_bin(Context, Line, Text) of
        <<>> ->
            <<>>;
        String ->
            Random = [{ephp_data:urand(), A} || <<A/utf8>> <= String],
            Sorted = lists:sort(Random),
            << <<A/utf8>> || {_, A} <- Sorted >>
    end.

-spec ucwords(ephp:context_id(), line(), var_value(), var_value()) -> binary().
ucwords(Context, Line, {_, Text}, {_, Sep}) ->
    SepList = binary_to_list(Sep),
    ephp_string:capitalize(
        ephp_data:to_bin(Context, Line, Text), SepList).

-spec str_split(ephp:context_id(), line(), Text :: var_value()) ->
                   ephp_array:ephp_array().
str_split(Context, Line, Text) ->
    str_split(Context, Line, Text, {1, 1}).

-spec str_split(ephp:context_id(), line(), Text :: var_value(), Size :: var_value()) ->
                   ephp_array:ephp_array() | undefined.
str_split(Context, Line, _Text, {_, Size}) when not is_integer(Size) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"str_split">>, 2, <<"long">>, ephp_data:gettype(Size)},
    ephp_error:handle_error(Context, {error, ewrongarg, Line, File, ?E_WARNING, Data}),
    undefined;
str_split(_Context, _Line, {_, Text}, {_, Size}) ->
    split_chars(Text, ephp_array:new(), 0, Size).

-spec print(ephp:context_id(), line(), [var_value()]) -> 1.
print(_Context, _Line, []) ->
    1;
print(Context, Line, [{_, #obj_ref{} = ObjRef} | Values]) ->
    ValueStr = ephp_data:to_bin(Context, Line, ObjRef),
    ephp_context:set_output(Context, ValueStr),
    print(Context, Line, Values);
print(Context, Line, [{_, Value} | Values]) ->
    ValueStr = ephp_data:to_bin(Value),
    ephp_context:set_output(Context, ValueStr),
    print(Context, Line, Values).

-spec strpos(ephp:context_id(),
             line(),
             HayStack :: var_value(),
             Needle :: var_value(),
             Offset :: var_value()) ->
                false | pos_integer() | undefined.
strpos(Context, Line, {_, HayStack}, {_, Needle}, {_, Offset}) ->
    Length = byte_size(HayStack) - Offset,
    if Length =< 0 orelse Offset < 0 ->
           File = ephp_context:get_active_file(Context),
           Error = {error, eoffset, Line, File, ?E_WARNING, {<<"strpos">>}},
           ephp_error:handle_error(Context, Error),
           undefined;
       true ->
           case binary:match(HayStack, Needle, [{scope, {Offset, Length}}]) of
               nomatch ->
                   false;
               {Pos, _Len} ->
                   Pos
           end
    end.

-spec strrpos(ephp:context_id(),
              line(),
              HayStack :: var_value(),
              Needle :: var_value(),
              Offset :: var_value()) ->
                 false | pos_integer() | undefined.
strrpos(Context, Line, {_, HayStack}, {_, Needle}, {_, Offset}) when Offset < 0 ->
    Length = byte_size(HayStack) + Offset + 1,
    if Length =< 0 ->
           File = ephp_context:get_active_file(Context),
           Error = {error, eoffset, Line, File, ?E_WARNING, {<<"strrpos">>}},
           ephp_error:handle_error(Context, Error),
           undefined;
       true ->
           RealLength = Length + byte_size(Needle) - 1,
           <<NewHayStack:RealLength/binary, _/binary>> = HayStack,
           RevHayStack = ephp_string:reverse(NewHayStack),
           RevNeedle = ephp_string:reverse(Needle),
           case binary:match(RevHayStack, RevNeedle) of
               nomatch ->
                   false;
               {Pos, _Len} ->
                   Length - Pos - 1
           end
    end;
strrpos(Context, Line, {_, HayStack}, {_, Needle}, {_, Offset}) ->
    Length = byte_size(HayStack) - Offset,
    if Length =< 0 ->
           File = ephp_context:get_active_file(Context),
           Error = {error, eoffset, Line, File, ?E_WARNING, {<<"strrpos">>}},
           ephp_error:handle_error(Context, Error),
           undefined;
       true ->
           <<_:Offset/binary, NewHayStack/binary>> = HayStack,
           RevHayStack = ephp_string:reverse(NewHayStack),
           RevNeedle = ephp_string:reverse(Needle),
           case binary:match(RevHayStack, RevNeedle) of
               nomatch ->
                   false;
               {Pos, _Len} ->
                   Length - Pos - 1 + Offset
           end
    end.

-spec strrev(ephp:context_id(), line(), Str :: var_value()) -> binary().
strrev(_Context, _Line, {_, Str}) ->
    ephp_string:reverse(Str).

-spec rtrim(ephp:context_id(), line(), Str :: var_value(), CharMask :: var_value()) ->
               binary() | undefined.
rtrim(_Context, _Line, {_, Str}, {_, CharMask}) ->
    Chars = binary_to_list(ephp_string:expand_mask(CharMask)),
    ephp_string:rtrim(Str, Chars).

-spec ltrim(ephp:context_id(), line(), Str :: var_value(), CharMask :: var_value()) ->
               binary() | undefined.
ltrim(_Context, _Line, {_, Str}, {_, CharMask}) ->
    Chars = binary_to_list(ephp_string:expand_mask(CharMask)),
    ephp_string:ltrim(Str, Chars).

-spec trim(ephp:context_id(), line(), Str :: var_value(), CharMask :: var_value()) ->
              binary() | undefined.
trim(_Context, _Line, {_, Str}, {_, CharMask}) ->
    Chars = binary_to_list(CharMask),
    ephp_string:trim(Str, Chars).

-spec md5(ephp:context_id(), line(), var_value(), var_value()) -> binary().
md5(_Context, _Line, {_, Binary}, {_, Raw}) ->
    case ephp_data:to_boolean(Raw) of
        true ->
            erlang:md5(Binary);
        false ->
            ephp_string:bin2hex(
                erlang:md5(Binary))
    end.

-spec bin2hex(ephp:context_id(), line(), var_value()) -> binary().
bin2hex(_Context, _Line, {_, Binary}) ->
    ephp_string:bin2hex(Binary).

-spec hex2bin(ephp:context_id(), line(), var_value()) -> binary().
hex2bin(Context, Line, {_, Hex}) when byte_size(Hex) rem 2 =/= 0 ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"hex2bin">>},
    ephp_error:handle_error(Context, {error, ehexeven, Line, File, ?E_WARNING, Data}),
    <<>>;
hex2bin(_Context, _Line, {_, Hex}) ->
    ephp_string:hex2bin(Hex).

-spec substr(ephp:context_id(), line(), var_value(), var_value(), var_value()) ->
                binary().
substr(_Context, _Line, {_, String}, {_, Start}, {_, Len}) ->
    limit(offset(String, Start), Len).

-spec str_pad(ephp:context_id(),
              line(),
              var_value(),
              var_value(),
              var_value(),
              var_value()) ->
                 binary().
str_pad(_Context, _Line, {_, Input}, {_, PadLen}, {_, PadStr}, {_, PadType}) ->
    case {byte_size(Input) > PadLen, PadType} of
        {true, _} ->
            Input;
        {false, ?STR_PAD_BOTH} ->
            ephp_string:pad(Input, PadLen, PadStr);
        {false, ?STR_PAD_LEFT} ->
            ephp_string:lpad(Input, PadLen, PadStr);
        {false, ?STR_PAD_RIGHT} ->
            ephp_string:rpad(Input, PadLen, PadStr)
    end.

-spec str_repeat(ephp:context_id(), line(), var_value(), var_value()) ->
                    binary() | undefined.
str_repeat(Context, Line, {_, _String}, {_, Multiplier}) when Multiplier < 0 ->
    File = ephp_context:get_active_file(Context),
    ephp_error:handle_error(Context,
                            {error, e2posint, Line, File, ?E_WARNING, {<<"str_repeat">>}}),
    undefined;
str_repeat(_Context, _Line, {_, String}, {_, Multiplier}) ->
    ephp_string:repeat(Multiplier, String).

-spec count_chars(ephp:context_id(), line(), var_value(), var_value()) ->
                     ephp_array:ephp_array() | binary().
count_chars(_Context, _Line, {_, String}, {_, 1}) ->
    % same as 0 but only byte-values with a frequency greater than zero are listed.
    lists:foldl(fun(I, Array) -> ephp_array:update_counter(I, 1, Array) end,
                ephp_array:new(),
                lists:sort(binary_to_list(String)));
count_chars(_Context, _Line, {_, String}, {_, 2}) ->
    % same as 0 but only byte-values with a frequency equal to zero are listed.
    FullArray = ephp_array:from_list([{I, 0} || I <- lists:seq(0, 255)]),
    lists:foldl(fun(I, Array) -> ephp_array:erase(I, Array) end,
                FullArray,
                lists:usort(binary_to_list(String)));
count_chars(_Context, _Line, {_, String}, {_, 3}) ->
    % string containing all unique characters is returned.
    << <<I:8>> || I <- lists:usort(binary_to_list(String)) >>;
count_chars(_Context, _Line, {_, String}, {_, 4}) ->
    % string containing all not used characters is returned.
    iolist_to_binary(lists:seq(0, 255) -- lists:usort(binary_to_list(String)));
count_chars(_Context, _Line, {_, String}, {_, _}) ->
    % an array with the byte-value as key and the frequency of every byte as value.
    FullArray = ephp_array:from_list([{I, 0} || I <- lists:seq(0, 255)]),
    lists:foldl(fun(I, Array) -> ephp_array:update_counter(I, 1, Array) end,
                FullArray,
                binary_to_list(String)).

-spec nl2br(ephp:context_id(), line(), var_value(), var_value()) -> binary().
nl2br(_Context, _Line, {_, String}, {_, Xhtml}) ->
    BR = case ephp_data:to_boolean(Xhtml) of
             true ->
                 <<"<br />&">>;
             false ->
                 <<"<br>&">>
         end,
    Opts = [global],
    re:replace(String, <<"(\\r\\n|\\n\\r|\\r|\\n)">>, BR, Opts).

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

offset(String, Start) when Start =:= 0 ->
    String;
offset(String, Start) when byte_size(String) =< abs(Start) ->
    String;
offset(String, Start) when Start > 0 ->
    binary_part(String, {Start, byte_size(String) - Start});
offset(String, Start) when Start < 0 ->
    binary_part(String, {byte_size(String) - abs(Start), abs(Start)}).

limit(String, eol) ->
    limit(String, byte_size(String));
limit(_String, Len) when Len =:= 0 ->
    <<>>;
limit(String, Len) when Len < 0 ->
    Size = min(byte_size(String), abs(Len)),
    binary_part(String, {0, byte_size(String) - Size});
limit(String, Len) when Len > 0 ->
    Size = min(byte_size(String), Len),
    binary_part(String, {0, Size}).

split_chars(<<>>, Parts, _I, _Size) ->
    Parts;
split_chars(String, Parts, I, Size) ->
    case String of
        <<Text:Size/binary, Rest/binary>> ->
            split_chars(Rest, ephp_array:store(I, Text, Parts), I + 1, Size);
        <<Text/binary>> ->
            ephp_array:store(I, Text, Parts)
    end.

split_limit(Delimiter, String, Parts, Limit, 0) when Limit < 0 ->
    BinParts = binary:split(String, Delimiter, [global]),
    case length(BinParts) + Limit of
        NumParts when NumParts >= 0 ->
            {_, NewParts} =
                lists:foldl(fun (Part, {I, NP}) when I =< NumParts ->
                                    {I + 1, ephp_array:store(auto, Part, NP)};
                                (_Part, Result) ->
                                    Result
                            end,
                            {0, Parts},
                            BinParts),
            NewParts;
        _ ->
            Parts
    end;
split_limit(_Delimiter, String, Parts, Limit, Limit) ->
    ephp_array:store(Limit, String, Parts);
split_limit(Delimiter, String, Parts, Limit, Index) ->
    case binary:split(String, Delimiter) of
        [Part, RestString] ->
            NewParts = ephp_array:store(Index, Part, Parts),
            split_limit(Delimiter, RestString, NewParts, Limit, Index + 1);
        [LastPart] ->
            ephp_array:store(Index, LastPart, Parts)
    end.

print_format(Format, Values) ->
    print_format(Format, {1, Values}, <<>>).

print_format(<<>>, _Values, Result) ->
    Result;
print_format(<<"%%", Rest/binary>>, Values, Result) ->
    print_format(Rest, Values, <<Result/binary, "%">>);
print_format(<<"%", _/binary>> = Text, Values, Result) ->
    E = "^%(\\d+\\$)?([+-]?)([ 0]|'.{1})?(-)?(\\d*)(\\.\\d+)?(.)",
    {NewValues, Output, Catched} =
        case re:run(Text, E, [{capture, all, binary}]) of
            {match, [Catched0, Order, Sign, Pad, Justify, Size, Decimal, Cmd]} ->
                {Value, Values0} = select_value(Order, Values),
                P = get_pad(Pad),
                J = get_justify(Justify),
                S = get_size(Size),
                D = get_decimal(Decimal),
                G = get_sign(Sign),
                Output0 = format(Cmd, G, P, J, S, D, Value),
                {Values0, Output0, Catched0}
        end,
    CatchedSize = byte_size(Catched),
    <<Catched:CatchedSize/binary, Rest/binary>> = Text,
    print_format(Rest, NewValues, <<Result/binary, Output/binary>>);
print_format(<<A/utf8, Rest/binary>>, Values, Result) ->
    print_format(Rest, Values, <<Result/binary, A/utf8>>).

select_value(<<>>, {I, Values}) ->
    {_, Val} = lists:nth(I, Values),
    {Val, {I + 1, Values}};
select_value(Pos, {I, Values}) ->
    NPos = ephp_data:bin_to_number(Pos),
    {_, Val} = lists:nth(NPos, Values),
    {Val, {I, Values}}.

get_pad(<<>>) ->
    32;
get_pad(<<Pad:8>>) ->
    Pad;
get_pad(<<"'", Pad:8>>) ->
    Pad.

get_decimal(<<>>) ->
    undefined;
get_decimal(<<".", Precision/binary>>) ->
    ephp_data:bin_to_number(Precision).

get_size(<<>>) ->
    ?PHP_INF;
get_size(Size) ->
    ephp_data:bin_to_number(Size).

get_justify(<<>>) ->
    right;
get_justify(<<"-">>) ->
    left.

get_sign(<<"+">>) ->
    true;
get_sign(_) ->
    false.

adjust(Text, _Pad, _Justify, ?PHP_INF) ->
    Text;
adjust(Text, Pad, Justify, Size) ->
    case Size - byte_size(Text) of
        PadSize when PadSize > 0 ->
            Padding = list_to_binary(lists:duplicate(PadSize, Pad)),
            case Justify of
                left ->
                    <<Text/binary, Padding/binary>>;
                right ->
                    <<Padding/binary, Text/binary>>
            end;
        _ ->
            Text
    end.

add_sign(true, Number, Binary) when Number >= 0 ->
    <<"+", Binary/binary>>;
add_sign(_, Number, Binary) when Number < 0 ->
    <<"-", Binary/binary>>;
add_sign(_, _, Binary) ->
    Binary.

format(<<"b">>, _Sign, Pad, Justify, Size, _Decimal, Value) ->
    Number = ephp_data:to_int(Value),
    Binary = integer_to_binary(trunc(Number), 2),
    adjust(Binary, Pad, Justify, Size);
format(<<"c">>, _Sign, _Pad, _Justify, _Size, _Decimal, Value) ->
    Number = ephp_data:to_int(Value),
    <<Number:8>>;
format(<<"d">>, Sign, Pad, Justify, Size, _Decimal, Value) ->
    Number = ephp_data:to_int(Value),
    Binary = integer_to_binary(abs(Number)),
    add_sign(Sign, Number, adjust(Binary, Pad, Justify, Size));
format(<<"e">>, Sign, Pad, Justify, Size, undefined, Value) ->
    Precision = ephp_config:get(<<"precision">>),
    Number = ephp_data:to_float(Value),
    Binary = float_to_binary(Number, [{scientific, Precision}, compact]),
    adjust(add_sign(Sign, Number, Binary), Pad, Justify, Size);
format(<<"e">>, Sign, Pad, Justify, Size, Precision, Value) ->
    Number = ephp_data:to_float(Value),
    Binary = float_to_binary(Number, [{scientific, Precision}, compact]),
    adjust(add_sign(Sign, Number, Binary), Pad, Justify, Size);
format(<<"E">>, Sign, Pad, Justify, Size, Decimal, Value) ->
    Result = format(<<"e">>, Sign, Pad, Justify, Size, Decimal, Value),
    ephp_string:to_upper(Result);
format(<<"f">>, Sign, Pad, Justify, Size, undefined, Value) ->
    Precision = ephp_config:get(<<"precision">>),
    Number = ephp_data:to_float(Value),
    Binary = float_to_binary(Number, [{decimals, Precision}]),
    adjust(add_sign(Sign, Number, Binary), Pad, Justify, Size);
format(<<"f">>, Sign, Pad, Justify, Size, Precision, Value) ->
    Number = ephp_data:to_float(Value),
    Binary = float_to_binary(Number, [{decimals, Precision}]),
    adjust(add_sign(Sign, Number, Binary), Pad, Justify, Size);
format(<<"F">>, Sign, Pad, Justify, Size, Decimal, Value) ->
    Result = format(<<"f">>, Sign, Pad, Justify, Size, Decimal, Value),
    ephp_string:to_upper(Result);
format(<<"g">>, Sign, Pad, Justify, Size, undefined, Value) ->
    Precision = ephp_config:get(<<"precision">>),
    Number = ephp_data:to_float(Value),
    Binary = float_to_binary(Number, [{decimals, Precision}, compact]),
    adjust(add_sign(Sign, Number, Binary), Pad, Justify, Size);
format(<<"g">>, Sign, Pad, Justify, Size, Precision, Value) ->
    Number = ephp_data:to_float(Value),
    Binary = float_to_binary(Number, [{decimals, Precision}, compact]),
    adjust(add_sign(Sign, Number, Binary), Pad, Justify, Size);
format(<<"G">>, Sign, Pad, Justify, Size, Decimal, Value) ->
    Result = format(<<"g">>, Sign, Pad, Justify, Size, Decimal, Value),
    ephp_string:to_upper(Result);
format(<<"o">>, _Sign, Pad, Justify, Size, _Decimal, Value) ->
    Number = ephp_data:to_int(Value),
    Binary = integer_to_binary(trunc(Number), 8),
    adjust(Binary, Pad, Justify, Size);
format(<<"s">>, _Sign, Pad, Justify, Size, _Decimal, Value) ->
    adjust(ephp_data:to_bin(Value), Pad, Justify, Size);
format(<<"u">>, _Sign, Pad, Justify, Size, _Decimal, Value) ->
    Number = ephp_data:to_int(Value),
    SNumber =
        if Number < 0 ->
               abs(?PHP_INT_MIN - Number);
           true ->
               Number
        end,
    Binary = integer_to_binary(SNumber),
    adjust(Binary, Pad, Justify, Size);
format(<<"X">>, _Sign, Pad, Justify, Size, _Decimal, Value) ->
    Number = ephp_data:to_int(Value),
    Binary = integer_to_binary(trunc(Number), 16),
    adjust(Binary, Pad, Justify, Size);
format(<<"x">>, Sign, Pad, Just, Size, Dec, Value) ->
    ephp_string:to_lower(format(<<"X">>, Sign, Pad, Just, Size, Dec, Value));
format(_Cmd, _Sign, _Pad, _Just, _Size, _Dec, _Value) ->
    <<>>.

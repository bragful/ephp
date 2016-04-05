-module(ephp_lib_string).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    strlen/3,
    ord/3,
    chr/3,
    implode/3,
    implode/4,
    explode/4,
    explode/5,
    str_replace/5,
    str_replace/6,
    strtolower/3,
    strtoupper/3,
    str_split/3,
    str_split/4
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    strlen, ord, chr,
    implode,
    {implode, [{alias, <<"join">>}]},
    explode,
    {explode, [{alias, <<"split">>}]},
    str_replace,
    strtolower,
    strtoupper,
    str_split
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec strlen(context(), line(), String :: var_value()) -> integer().

strlen(_Context, _Line, {_,String}) when is_binary(String) ->
    byte_size(String);

strlen(Context, Line, {_, Var}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"strlen">>, 1, <<"string">>, ephp_util:gettype(Var), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec ord(context(), line(), String :: var_value()) -> integer().

ord(_Context, _Line, {_,<<I:8/integer,_/binary>>}) ->
    I;

ord(Context, Line, {_, Var}) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"ord">>, 1, <<"string">>, ephp_util:gettype(Var), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined.

-spec chr(context(), line(), Integer :: var_value()) -> binary().

chr(_Context, _Line, {_,C}) when is_integer(C) ->
    <<C:8/integer>>;

chr(_Context, _Line, _Var) ->
    undefined.

-spec implode(context(), line(),
    Glue :: var_value(), Pieces :: var_value()) -> binary().

implode(Context, Line, {_,Glue}=VarGlue, _Pieces) when ?IS_ARRAY(Glue) ->
    File = ephp_context:get_active_file(Context),
    Data = {File, <<"string">>},
    ephp_error:handle_error(Context, {error, earrayconv, Line, ?E_NOTICE, Data}),
    implode(Context, Line, {undefined, <<"Array">>}, VarGlue);

implode(Context, Line, {_,RawGlue}, {_,Pieces}) ->
    Glue = ephp_util:to_bin(Context, Line, RawGlue),
    ListOfPieces = ephp_array:fold(fun(_Key, Piece, SetOfPieces) ->
        SetOfPieces ++ [ephp_util:to_bin(Context, Line, Piece)]
    end, [], Pieces),
    case ListOfPieces of
        [] -> <<>>;
        [H|T] -> <<H/binary, (<< <<Glue/binary,X/binary>> || X <- T >>)/binary>>
    end.

-spec implode(context(), line(), Pieces :: var_value()) -> binary().

implode(Context, Line, Pieces) ->
    implode(Context, Line, {undefined, <<>>}, Pieces).

-spec explode(context(), line(), Delimiter :: var_value(), String :: var_value())
    -> ephp_array().

explode(_Context, _Line, {_,Delimiter}, {_,String}) ->
    Pieces = binary:split(String, Delimiter, [global]),
    lists:foldl(fun(Piece, Explode) ->
        ephp_array:store(auto, Piece, Explode)
    end, ephp_array:new(), Pieces).

-spec explode(
    context(), line(),
    Delimiter :: var_value(),
    String :: var_value(),
    Limit :: var_value()) -> ephp_array().

explode(Context, Line, _Delimiter, _String, {_,Limit})
        when not is_integer(Limit) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"explode">>, 3, <<"long">>, ephp_util:gettype(Limit), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined;

explode(_Context, _Line, _Delimiter, {_,String}, {_,0}) ->
    String;

explode(_Context, _Line, {_,Delimiter}, {_,String}, {_,Limit}) ->
    split_limit(Delimiter, String, ephp_array:new(), Limit-1, 0).

-spec str_replace(context(), line(),
    Search :: var_value(), Replace :: var_value(),
    Subject :: var_value()) -> binary().

str_replace(_Context, _Line, {_, Search}, {_, Replace}, {_, Subject})
        when ?IS_ARRAY(Search) andalso is_binary(Replace) ->
    ephp_array:fold(fun(_,V,Sub) ->
        binary:replace(Sub, V, Replace, [global])
    end, Subject, Search);

str_replace(_Context, _Line, {_, Search}, {_, Replace}, {_, Subject})
        when ?IS_ARRAY(Search) andalso ?IS_ARRAY(Replace) ->
    A = ephp_array:to_list(Search),
    B = ephp_array:to_list(Replace),
    Lists = lists:zipwith(fun({_,V1}, {_,V2}) -> {V1,V2} end, A, B),
    lists:foldl(fun({Source, Target}, Sub) ->
        binary:replace(Sub, Source, Target, [global])
    end, Subject, Lists);

str_replace(_Context, _Line, {_, Search}, {_, Replace}, {_, Subject}) ->
    binary:replace(Subject, Search, Replace, [global]).

-spec str_replace(context(), line(),
    Search :: var_value(), Replace :: var_value(),
    Subject :: var_value(), Count :: var_value()) -> binary().

str_replace(Context, _Line, {_, Search}, {_, Replace}, {_, Subject}, {Count,_}) ->
    ephp_context:set(Context, Count, length(binary:matches(Subject, Search))),
    binary:replace(Subject, Search, Replace, [global]).

-spec strtolower(context(), line(), Text :: var_value()) -> binary().

strtolower(Context, Line, {_, Text}) ->
    unistring:to_lower(ephp_util:to_bin(Context, Line, Text)).

-spec strtoupper(context(), line(), Text :: var_value()) -> binary().

strtoupper(Context, Line, {_, Text}) ->
    unistring:to_upper(ephp_util:to_bin(Context, Line, Text)).

-spec str_split(context(), line(), Text :: var_value()) -> ephp_array().

str_split(Context, Line, Text) ->
    str_split(Context, Line, Text, {1, 1}).

-spec str_split(context(), line(),
    Text :: var_value(), Size :: var_value()) -> ephp_array() | undefined.

str_split(Context, Line, _Text, {_, Size}) when not is_integer(Size) ->
    File = ephp_context:get_active_file(Context),
    Data = {<<"str_split">>, 2, <<"long">>, ephp_util:gettype(Size), File},
    ephp_error:handle_error(Context, {error, ewrongarg, Line,
        ?E_WARNING, Data}),
    undefined;

str_split(_Context, _Line, {_, Text}, {_, Size}) ->
    split_chars(Text, ephp_array:new(), 0, Size).


%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

split_chars(<<>>, Parts, _I, _Size) ->
    Parts;

split_chars(String, Parts, I, Size) ->
    case String of
        <<Text:Size/binary,Rest/binary>> ->
            split_chars(Rest, ephp_array:store(I, Text, Parts), I+1, Size);
        <<Text/binary>> ->
            ephp_array:store(I, Text, Parts)
    end.

split_limit(Delimiter, String, Parts, Limit, 0) when Limit < 0 ->
    BinParts = binary:split(String, Delimiter, [global]),
    case length(BinParts) + Limit of
        NumParts when NumParts >= 0 ->
            {_,NewParts} = lists:foldl(fun
                (Part, {I,NP}) when I =< NumParts ->
                    {I+1, ephp_array:store(auto, Part, NP)};
                (_Part, Result) ->
                    Result
            end, {0,Parts}, BinParts),
            NewParts;
        _ ->
            Parts
    end;

split_limit(_Delimiter, String, Parts, Limit, Limit) ->
    ephp_array:store(Limit, String, Parts);

split_limit(Delimiter, String, Parts, Limit, Index) ->
    case binary:split(String, Delimiter) of
        [Part,RestString] ->
            NewParts = ephp_array:store(Index, Part, Parts),
            split_limit(Delimiter, RestString, NewParts, Limit, Index+1);
        [LastPart] ->
            ephp_array:store(Index, LastPart, Parts)
    end.

-module(ephp_func_string).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    strlen/3,
    ord/3,
    chr/3,
    implode/3,
    implode/4,
    explode/4,
    explode/5
]).

-include("ephp.hrl").

-spec init() -> [ephp_func:php_function()].

init() -> [
    strlen, ord, chr,
    implode,
    {implode, <<"join">>},
    explode,
    {explode, <<"split">>}
]. 

-spec strlen(context(), line(), String :: var_value()) -> integer().

strlen(_Context, _Line, {_,String}) when is_binary(String) ->
    byte_size(String);

strlen(_Context, _Line, _Var) ->
    %% TODO: Warning: strlen() expects parameter 1 to be string
    null.

-spec ord(context(), line(), String :: var_value()) -> integer().

ord(_Context, _Line, {_,<<I:8/integer,_/binary>>}) ->
    I;

ord(_Context, _Line, _Var) ->
    %% TODO: Warning: ord() expects parameter 1 to be string
    null.

-spec chr(context(), line(), Integer :: var_value()) -> binary().

chr(_Context, _Line, {_,C}) when is_integer(C) ->
    <<C:8/integer>>;

chr(_Context, _Line, _Var) ->
    null.

-spec implode(context(), line(),
    Glue :: var_value(), Pieces :: var_value()) -> binary().

implode(Context, Line, {_,Glue}=VarGlue, _Pieces) when ?IS_DICT(Glue) ->
    implode(Context, Line, VarGlue);

implode(_Context, _Line, {_,RawGlue}, {_,Pieces}) ->
    Glue = ephp_util:to_bin(RawGlue),
    ListOfPieces = ?DICT:fold(fun(_Key, Piece, SetOfPieces) -> 
        SetOfPieces ++ [ephp_util:to_bin(Piece)]
    end, [], Pieces),
    case ListOfPieces of
        [] -> <<>>;
        [H|T] -> <<H/binary, (<< <<Glue/binary,X/binary>> || X <- T >>)/binary>>
    end.

-spec implode(context(), line(), Pieces :: var_value()) -> binary().

implode(Context, Line, Pieces) ->
    implode(Context, Line, <<>>, Pieces).

-spec explode(context(), line(), Delimiter :: var_value(), String :: var_value())
    -> ?DICT_TYPE.

explode(_Context, _Line, {_,Delimiter}, {_,String}) ->
    Pieces = binary:split(String, Delimiter, [global]),
    {_,Res} = lists:foldl(fun(Piece, {I,Explode}) ->
        {I+1, ?DICT:store(I, Piece, Explode)}
    end, {0,?DICT:new()}, Pieces),
    Res.

-spec explode(
    context(), line(),
    Delimiter :: var_value(),
    String :: var_value(),
    Limit :: var_value()) -> ?DICT_TYPE.

explode(_Context, _Line, _Delimiter, _String, {_,Limit})
        when not is_integer(Limit) ->
    %% TODO: Warning: explode() expects parameter 3 to be long
    null;

explode(_Context, _Line, _Delimiter, {_,String}, {_,0}) ->
    String;

explode(_Context, _Line, {_,Delimiter}, {_,String}, {_,Limit}) ->
    split_limit(Delimiter, String, ?DICT:new(), Limit-1, 0).


%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

split_limit(_Delimiter, String, Parts, Limit, Limit) ->
    ?DICT:store(Limit, String, Parts);

split_limit(Delimiter, String, Parts, Limit, Index) ->
    case binary:split(String, Delimiter) of
        [Part,RestString] ->
            NewParts = ?DICT:store(Index, Part, Parts),
            split_limit(Delimiter, RestString, NewParts, Limit, Index+1);
        [LastPart] ->
            ?DICT:store(Index, LastPart, Parts)
    end.

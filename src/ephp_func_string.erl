-module(ephp_func_string).
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    strlen/2,
    ord/2,
    chr/2,
    implode/2,
    implode/3,
    join/2,
    join/3
]).

-include("ephp.hrl").

-spec init() -> [ephp_func:php_function()].

init() -> [
    strlen, ord, chr, implode, join
]. 

-spec strlen(Context :: context(), String :: var_value()) -> integer().

strlen(_Context, {_,String}) when is_binary(String) ->
    byte_size(String);

strlen(_Context, _Var) ->
    %% TODO: Warning: strlen() expects parameter 1 to be string
    null.

-spec ord(Context :: context(), String :: var_value()) -> integer().

ord(_Context, {_,<<I:8/integer,_/binary>>}) ->
    I;

ord(_Context, _Var) ->
    %% TODO: Warning: ord() expects parameter 1 to be string
    null.

-spec chr(Context :: context(), Integer :: var_value()) -> binary().

chr(_Context, {_,C}) when is_integer(C) ->
    <<C:8/integer>>;

chr(_Context, _Var) ->
    null.

-spec implode(Context :: context(), Glue :: var_value(), Pieces :: var_value()) -> binary().

implode(Context, {_,Glue}=VarGlue, _Pieces) when ?IS_DICT(Glue) ->
    implode(Context, VarGlue);

implode(_Context, {_,RawGlue}, {_,Pieces}) ->
    Glue = ephp_util:to_bin(RawGlue),
    ListOfPieces = ?DICT:fold(fun(_Key, Piece, SetOfPieces) -> 
        SetOfPieces ++ [ephp_util:to_bin(Piece)]
    end, [], Pieces),
    case ListOfPieces of
        [] -> <<>>;
        [H|T] -> <<H/binary, (<< <<Glue/binary,X/binary>> || X <- T >>)/binary>>
    end.

-spec implode(Context :: context(), Pieces :: var_value()) -> binary().

implode(Context, Pieces) ->
    implode(Context, <<>>, Pieces).

-spec join(Context :: context(), Glue :: var_value(), Pieces :: var_value()) -> binary().

join(Context, Glue, Pieces) ->
    implode(Context, Glue, Pieces).

-spec join(Context :: context(), Pieces :: var_value()) -> binary().

join(Context, Pieces) ->
    implode(Context, Pieces).


%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------


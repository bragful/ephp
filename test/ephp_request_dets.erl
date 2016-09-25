-module(ephp_request_dets).
-compile([export_all]).

-define(TABLE, request_ets).

-include_lib("eunit/include/eunit.hrl").
-include("ephp.hrl").

ensure_array_exists(_ID) ->
    case dets:info(?TABLE) of
        undefined ->
            dets:open_file(?TABLE, [
                {type, set},
                {file, ?TABLE}
            ]),
            ?TABLE;
        _ ->
            ?TABLE
    end.

handle_array(ArrayID, _Array, {retrieve, Key}) ->
    ID = ensure_array_exists(ArrayID),
    case dets:lookup(ID, Key) of
        [{Key,Value}] -> {ok, Value};
        [] -> error
    end;

handle_array(ArrayID, Array, {Action, Key, Value})
        when Action =:= add; Action =:= update ->
    ID = ensure_array_exists(ArrayID),
    ok = dets:insert(ID, {Key, Value}),
    Array;

handle_array(ArrayID, Array, {remove, _Key}) ->
    _ID = ensure_array_exists(ArrayID),
    % ignore remove to use the same values for all of the tests
    % ok = dets:delete(ID, Key),
    Array;

handle_array(ArrayID, _Array, {fold, Fun, Initial}) ->
    ID = ensure_array_exists(ArrayID),
    dets:foldl(Fun, Initial, ID);

handle_array(ArrayID, _Array, to_list) ->
    ID = ensure_array_exists(ArrayID),
    dets:foldl(fun(D, Acc) -> Acc ++ [D] end, [], ID).

-module(ephp_func_vars).
-compile([warnings_as_errors]).

-export([
    init/1,
    is_bool/2,
    print_r/2,
    print_r/3,
    isset/2,
    empty/2
]).

-include("ephp.hrl").

-spec init(Context :: context()) -> ok.

init(Context) ->
    Funcs = [
        is_bool, print_r, isset, empty
    ],
    lists:foreach(fun(Func) ->
        Name = atom_to_binary(Func, utf8),
        ephp_context:register_func(Context, Name, ?MODULE, Func)  
    end, Funcs), 
    ok. 

-spec is_bool(Context :: context(), Value :: mixed()) -> boolean().

is_bool(_Context, Value) when is_boolean(Value) -> 
    true;

is_bool(_Context, _Value) -> 
    false.


-spec print_r(Context :: context(), Value :: mixed()) -> boolean().

print_r(_Context, Value) when not ?IS_DICT(Value) -> 
    ephp_util:to_bin(Value);

print_r(Context, Value) ->
    print_r(Context, Value, false).


-spec print_r(Context :: context(), Value :: mixed(), Output :: boolean()) -> null | binary().

print_r(_Context, Value, true) when not ?IS_DICT(Value) -> 
    ephp_util:to_bin(Value);

print_r(Context, Value, false) when not ?IS_DICT(Value) -> 
    ephp_context:set_output(Context, ephp_util:to_bin(Value)),
    null;

print_r(_Context, Value, true) ->
    Data = lists:foldl(fun(Chunk,Total) ->
        <<Total/binary, Chunk/binary>>
    end, <<>>, print_r_fmt(Value, <<"    ">>)),
    <<"Array\n(\n", Data/binary, ")\n">>;

print_r(Context, Value, false) ->
    Data = lists:foldl(fun(Chunk,Total) ->
        <<Total/binary, Chunk/binary>>
    end, <<>>, print_r_fmt(Value, <<"    ">>)),
    ephp_context:set_output(Context, <<"Array\n(\n", Data/binary, ")\n">>),
    null.

-spec isset(Context :: context(), Value :: mixed()) -> boolean().

isset(_Context, Value) ->
    case Value of
        undefined -> false;
        _ -> true
    end.

-spec empty(Context :: context(), Value :: mixed()) -> boolean().

empty(_Context, Value) ->
    case Value of
        undefined -> true;
        <<"0">> -> true;
        <<>> -> true;
        false -> true;
        _ -> false
    end.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

print_r_fmt(Value, _Spaces) when not ?IS_DICT(Value) -> 
    <<(ephp_util:to_bin(Value))/binary, "\n">>;

print_r_fmt(Value, Spaces) ->
    ?DICT:fold(fun(Key, Val, Res) ->
        KeyBin = ephp_util:to_bin(Key),
        Res ++ case print_r_fmt(Val, <<Spaces/binary, "    ">>) of
            V when is_binary(V) -> 
                [<<Spaces/binary, "[", KeyBin/binary, "] => ", V/binary>>];
            V when is_list(V) ->
                Content = lists:map(fun(Element) ->
                    <<Spaces/binary, Element/binary>>
                end, V),
                [
                    <<Spaces/binary, "[", KeyBin/binary, "] => Array\n">>, 
                    <<Spaces/binary, "(\n">>
                ] ++ Content ++ [
                    <<Spaces/binary, ")\n">>
                ]
        end
    end, [], Value).

-module(ephp_func_vars).
-compile([warnings_as_errors]).

-export([
    init/1,
    is_bool/2,
    is_integer/2,
    print_r/2,
    print_r/3,
    isset/2,
    empty/2,
    gettype/2,
    unset/2,
    define/3
]).

-include("ephp.hrl").

-spec init(Context :: context()) -> ok.

init(Context) ->
    Funcs = [
        is_bool, is_integer, print_r, isset, empty, gettype, unset, define
    ],
    lists:foreach(fun(Func) ->
        Name = atom_to_binary(Func, utf8),
        ephp_context:register_func(Context, Name, ?MODULE, Func)  
    end, Funcs), 
    ok. 

-spec is_bool(Context :: context(), Value :: var_value()) -> boolean().

is_bool(_Context, {_,Value}) when is_boolean(Value) -> 
    true;

is_bool(_Context, _Value) -> 
    false.

-spec is_integer(Context :: context(), Value :: var_value()) -> boolean().

is_integer(_Context, {_,Value}) when is_integer(Value) ->
    true;

is_integer(_Context, _Value) ->
    false.

-spec print_r(Context :: context(), Value :: var_value()) -> boolean().

print_r(_Context, {_,Value}) when not ?IS_DICT(Value) -> 
    ephp_util:to_bin(Value);

print_r(Context, Value) ->
    print_r(Context, Value, {false,false}).


-spec print_r(Context :: context(), Value :: var_value(), Output :: boolean()) -> null | binary().

print_r(_Context, {_,Value}, {_,true}) when not ?IS_DICT(Value) -> 
    ephp_util:to_bin(Value);

print_r(Context, {_,Value}, {_,false}) when not ?IS_DICT(Value) -> 
    ephp_context:set_output(Context, ephp_util:to_bin(Value)),
    null;

print_r(_Context, {_,Value}, {_,true}) ->
    Data = lists:foldl(fun(Chunk,Total) ->
        <<Total/binary, Chunk/binary>>
    end, <<>>, print_r_fmt(Value, <<"    ">>)),
    <<"Array\n(\n", Data/binary, ")\n">>;

print_r(Context, {_,Value}, {_,false}) ->
    Data = lists:foldl(fun(Chunk,Total) ->
        <<Total/binary, Chunk/binary>>
    end, <<>>, print_r_fmt(Value, <<"    ">>)),
    ephp_context:set_output(Context, <<"Array\n(\n", Data/binary, ")\n">>),
    null.

-spec isset(Context :: context(), Value :: var_value()) -> boolean().

isset(_Context, {_,Value}) ->
    case Value of
        undefined -> false;
        _ -> true
    end.

-spec empty(Context :: context(), Value :: var_value()) -> boolean().

empty(_Context, {_,Value}) ->
    case Value of
        undefined -> true;
        <<"0">> -> true;
        <<>> -> true;
        false -> true;
        _ -> false
    end.

-spec gettype(Context :: context(), Value :: var_value()) -> binary().

gettype(_Context, {_,Value}) ->
    if
        is_boolean(Value) -> <<"boolean">>;
        is_integer(Value) -> <<"integer">>;
        is_float(Value) -> <<"double">>;
        is_binary(Value) -> <<"string">>;
        ?IS_DICT(Value) -> <<"array">>;
        %% TODO: object type
        %% TODO: resource type
        Value =:= null -> <<"NULL">>;
        true -> <<"unknown type">>
    end.

-spec unset(Context :: context(), Var :: var_value()) -> null.

unset(Context, {Var,_}) ->
    ephp_context:set(Context, Var, undefined),
    null. 

-spec define(Context :: context(), Constant :: var_value(), Content :: var_value()) -> null.

define(Context, {#constant{name=Constant},_}, {_UnParsedContent,Content}) ->
    ephp_context:register_const(Context, Constant, Content),
    null.

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

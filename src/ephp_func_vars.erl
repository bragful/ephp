-module(ephp_func_vars).
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init/0,
    php_is_array/2,
    php_is_bool/2,
    php_is_integer/2,
    php_is_float/2,
    php_is_numeric/2,
    % php_is_null/2,
    php_is_object/2,
    php_is_string/2,
    print_r/2,
    var_dump/2,
    print_r/3,
    isset/2,
    empty/2,
    gettype/2,
    unset/2
]).

-include("ephp.hrl").

-define(SPACES, "    ").
-define(SPACES_VD, "  ").

-spec init() -> [ephp_func:php_function()].

init() -> [
    {php_is_array, <<"is_array">>},
    {php_is_bool, <<"is_bool">>},
    {php_is_integer, <<"is_long">>},
    {php_is_integer, <<"is_int">>},
    {php_is_integer, <<"is_integer">>},
    {php_is_float, <<"is_float">>},
    {php_is_float, <<"is_double">>},
    {php_is_numeric, <<"is_numeric">>},
    % {php_is_null, <<"is_null">>},
    {php_is_object, <<"is_object">>},
    {php_is_string, <<"is_string">>},
    print_r,
    isset,
    empty,
    gettype,
    unset,
    var_dump
].

-spec php_is_array(context(), var_value()) -> boolean().
php_is_array(_Context, {_,Value}) -> ?IS_DICT(Value).

-spec php_is_bool(context(), var_value()) -> boolean().
php_is_bool(_Context, {_,Value}) -> erlang:is_boolean(Value).

-spec php_is_integer(context(), var_value()) -> boolean().
php_is_integer(_Context, {_,Value}) -> erlang:is_integer(Value).

-spec php_is_numeric(context(), var_value()) -> boolean().
php_is_numeric(_Context, {_,Value}) -> erlang:is_number(Value).

-spec php_is_float(context(), var_value()) -> boolean().
php_is_float(_Context, {_,Value}) -> erlang:is_float(Value).

% -spec php_is_null(context(), var_value()) -> boolean().
% php_is_null(_Context, {_,Value}) -> Value =:= null orelse Value =:= undefined.

-spec php_is_string(context(), var_value()) -> boolean().
php_is_string(_Context, {_,Value}) -> erlang:is_binary(Value).

-spec php_is_object(context(), var_value()) -> boolean().
php_is_object(_Context, {_,Value}) -> erlang:is_record(Value, reg_instance).

-spec print_r(context(), var_value()) -> true | binary().

print_r(Context, {_,#reg_instance{}}=Vars) ->
    print_r(Context, Vars, {false,false});

print_r(_Context, {_,Value}) when not ?IS_DICT(Value) -> 
    ephp_util:to_bin(Value);

print_r(Context, Value) ->
    print_r(Context, Value, {false,false}).


-spec var_dump(context(), var_value()) -> null.

var_dump(Context, {_,Value}) ->
    Result = case var_dump_fmt(Context, Value, <<?SPACES_VD>>) of
    Elements when is_list(Elements) ->
        Data = lists:foldl(fun(Chunk,Total) ->
            <<Total/binary, Chunk/binary>>
        end, <<>>, Elements),
        Size = case Value of
        V when is_list(V) ->
            ephp_util:to_bin(length(Value));
        #reg_instance{class=#class{attrs=Attrs}} ->
            ephp_util:to_bin(length(Attrs))
        end,
        if ?IS_DICT(Value) ->
            <<"array(", Size/binary, ") {\n", Data/binary, "}\n">>;
        is_record(Value, reg_instance) ->
            #reg_instance{class=Class} = Value,
            %% TODO: add instance number (ID)
            <<"class ", (Class#class.name)/binary, " (", Size/binary, ") ",
              "{\n", Data/binary, "}\n">>;
        true ->
            Data
        end;
    Element ->
        Element
    end,
    ephp_context:set_output(Context, Result), 
    null.

-spec print_r(context(), var_value(), Output :: boolean()) -> true | binary().

print_r(_Context, {_,#reg_instance{class=Class, context=Ctx}}, {_,true}) ->
    Data = lists:foldl(fun(#class_attr{name=Name}, Output) ->
        Value = ephp_context:get(Ctx, #variable{name=Name}), 
        ValDumped = print_r_fmt(Ctx, Value, <<?SPACES>>),
        <<Output/binary, ?SPACES, "[", Name/binary, "] => ", 
          ValDumped/binary, "\n">>
    end, <<>>, Class#class.attrs),
    <<(Class#class.name)/binary, " Object\n(\n", Data/binary, ")\n">>;

print_r(Context, {_,#reg_instance{class=Class, context=Ctx}}, {_,false}) ->
    Data = lists:foldl(fun(#class_attr{name=Name}, Output) ->
        Value = ephp_context:get(Ctx, #variable{name=Name}), 
        ValDumped = print_r_fmt(Ctx, Value, <<?SPACES>>),
        <<Output/binary, ?SPACES, "[", Name/binary, "] => ", 
          ValDumped/binary>>
    end, <<>>, Class#class.attrs),
    Out = <<(Class#class.name)/binary, " Object\n(\n", Data/binary, ")\n">>,
    ephp_context:set_output(Context, Out),
    true; 

print_r(_Context, {_,Value}, {_,true}) when not ?IS_DICT(Value) -> 
    ephp_util:to_bin(Value);

print_r(Context, {_,Value}, {_,false}) when not ?IS_DICT(Value) -> 
    ephp_context:set_output(Context, ephp_util:to_bin(Value)),
    true;

print_r(Context, {_,Value}, {_,true}) ->
    Data = lists:foldl(fun(Chunk,Total) ->
        <<Total/binary, Chunk/binary>>
    end, <<>>, print_r_fmt(Context, Value, <<?SPACES>>)),
    <<"Array\n(\n", Data/binary, ")\n">>;

print_r(Context, {_,Value}, {_,false}) ->
    Data = lists:foldl(fun(Chunk,Total) ->
        <<Total/binary, Chunk/binary>>
    end, <<>>, print_r_fmt(Context, Value, <<?SPACES>>)),
    ephp_context:set_output(Context, <<"Array\n(\n", Data/binary, ")\n">>),
    true.

-spec isset(context(), var_value()) -> boolean().

isset(_Context, {_,Value}) ->
    case Value of
        undefined -> false;
        _ -> true
    end.

-spec empty(context(), var_value()) -> boolean().

empty(_Context, {_,Value}) ->
    case Value of
        undefined -> true;
        <<"0">> -> true;
        <<>> -> true;
        false -> true;
        _ -> false
    end.

-spec gettype(context(), var_value()) -> binary().

gettype(_Context, {_,Value}) when is_boolean(Value) -> <<"boolean">>;
gettype(_Context, {_,Value}) when is_integer(Value) -> <<"integer">>;
gettype(_Context, {_,Value}) when is_float(Value) -> <<"double">>;
gettype(_Context, {_,Value}) when is_binary(Value) -> <<"string">>;
gettype(_Context, {_,Value}) when ?IS_DICT(Value) -> <<"array">>;
gettype(_Context, {_,Value}) when is_record(Value, reg_instance) -> <<"object">>;
gettype(_Context, {_,Value}) when is_pid(Value) -> <<"resource">>;
gettype(_Context, {_,null}) -> <<"NULL">>;
gettype(_Context, {_,_}) -> <<"unknown type">>.

-spec unset(context(), var_value()) -> null.

unset(Context, {Var,_}) ->
    %% TODO: find objects in remove data to run __destruct if it's defined.
    ephp_context:set(Context, Var, undefined),
    null. 

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------

var_dump_fmt(Context, {var_ref,VarPID,VarRef}, Spaces) ->
    %% FIXME add recursion control
    Var = ephp_vars:get(VarPID, VarRef),
    var_dump_fmt(Context, Var, Spaces);

var_dump_fmt(_Context, true, _Spaces) ->
    <<"bool(true)\n">>;

var_dump_fmt(_Context, false, _Spaces) ->
    <<"bool(false)\n">>;

var_dump_fmt(_Context, Value, _Spaces) when is_integer(Value) -> 
    <<"int(",(ephp_util:to_bin(Value))/binary, ")\n">>;

var_dump_fmt(_Context, Value, _Spaces) when is_float(Value) -> 
    <<"double(",(ephp_util:to_bin(Value))/binary, ")\n">>;

var_dump_fmt(_Context, Value, _Spaces) when is_binary(Value) -> 
    Size = ephp_util:to_bin(byte_size(Value)),
    <<"string(",Size/binary,") \"",(ephp_util:to_bin(Value))/binary, "\"\n">>;

var_dump_fmt(Context, #reg_instance{class=Class, context=Ctx}, Spaces) ->
    lists:foldl(fun(#class_attr{name=Name,access=Acc}, Output) ->
        Access = atom_to_binary(Acc, utf8),
        Value = ephp_context:get(Ctx, #variable{name=Name}), 
        ValDumped = var_dump_fmt(Context, Value, <<Spaces/binary, ?SPACES_VD>>),
        Output ++ [<<
          Spaces/binary, Access/binary, " $", Name/binary, " =>\n",
          Spaces/binary, ValDumped/binary>>]
    end, [], Class#class.attrs);

var_dump_fmt(Context, Value, Spaces) ->
    ?DICT:fold(fun(Key, Val, Res) ->
        KeyBin = if
            not is_binary(Key) -> ephp_util:to_bin(Key);
            true -> <<"\"", Key/binary, "\"">>
        end,
        Res ++ case var_dump_fmt(Context, Val, <<Spaces/binary, ?SPACES_VD>>) of
            V when is_binary(V) -> 
                [
                    <<Spaces/binary, "[", KeyBin/binary, "] =>\n",
                        Spaces/binary, V/binary>>
                ];
            V when is_list(V) ->
                Elements = ephp_util:to_bin(length(Val)),
                [
                    <<Spaces/binary, "[", KeyBin/binary, "] =>\n">>,
                    <<Spaces/binary,"array(", Elements/binary, ") {\n">>
                ] ++ V ++ [
                    <<Spaces/binary, "}\n">>
                ]
        end
    end, [], Value).

print_r_fmt(Context, {var_ref,VarPID,VarRef}, Spaces) ->
    %% FIXME add recursion control
    Var = ephp_vars:get(VarPID, VarRef),
    print_r_fmt(Context, Var, Spaces);

print_r_fmt(_Context, Value, _Spaces) when not ?IS_DICT(Value) -> 
    <<(ephp_util:to_bin(Value))/binary, "\n">>;

print_r_fmt(Context, Value, Spaces) ->
    ?DICT:fold(fun(Key, Val, Res) ->
        KeyBin = ephp_util:to_bin(Key),
        Res ++ case print_r_fmt(Context, Val, <<Spaces/binary, ?SPACES>>) of
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

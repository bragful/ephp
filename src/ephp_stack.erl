-module(ephp_stack).
-author('manuel@altenwald.com').
-compile([warnings_as_errors, {no_auto_import, [get/1]}]).

-include("ephp.hrl").

-export([
    start_link/1,
    destroy/1,
    get/1,
    get_array/1,
    push/7,
    pop/1
]).

start_link(Ref) ->
    case get(Ref) of
        Stack when is_list(Stack) ->
            ok;
        undefined ->
            erlang:put(get_id(Ref), [])
    end,
    {ok, get_id(Ref)}.

destroy(Ref) ->
    erlang:erase(get_id(Ref)).

push(_Ref, _File, undefined, _Fun, _Args, _Class, _Object) ->
    ok;
push(Ref, File, {{line,Line},_}, Fun, Args, Class, Object) ->
    Stack = get(Ref),
    Type = if
        Class =:= undefined -> undefined;
        Object =:= undefined -> <<"::">>;
        true -> <<"->">>
    end,
    New = #stack_trace{
        function = Fun,
        args = Args,
        file = File,
        line = Line,
        object = Object,
        class = Class,
        type = Type
    },
    erlang:put(get_id(Ref), [New|Stack]),
    ok.

pop(Ref) ->
    case get(Ref) of
        [Head|Stack] ->
            erlang:put(get_id(Ref), Stack),
            Head;
        [] ->
            undefined
    end.

get(Ref) ->
    erlang:get(get_id(Ref)).

get_array(Ref) ->
    lists:foldl(fun(Stack, Array) ->
        #stack_trace{
            function = Fun,
            file = File,
            line = Line,
            class = Class,
            object = Object,
            type = Type,
            args = Args
        } = Stack,
        Element = ephp_array:from_list(
        [
            {<<"file">>, File},
            {<<"line">>, Line}
        ] ++ add_function(Fun)
          ++ add_class(Class)
          ++ add_object(Object)
          ++ add_type(Type)
          ++ add_args(Fun, Args)),
        ephp_array:store(auto, Element, Array)
    end, ephp_array:new(), get(Ref)).

add_function(Fun) ->
    Incs = [<<"include">>, <<"include_once">>,
            <<"require">>, <<"require_once">>],
    case lists:member(ephp_string:to_lower(Fun), Incs) of
        true -> [];
        false -> [{<<"function">>, Fun}]
    end.

add_class(undefined) -> [];
add_class(Class) -> [{<<"class">>, Class}].

add_object(undefined) -> [];
add_object(Object) -> [{<<"object">>, Object}].

add_type(undefined) -> [];
add_type(Type) -> [{<<"type">>, Type}].

add_args(Fun, Args) ->
    [{<<"args">>, ephp_array:from_list(Args)}] ++
    case add_function(Fun) of
        [] -> [{<<"function">>, Fun}];
        _ -> []
    end.

get_id(_Ref) ->
    % FIXME: at this moment it's very complicated to get only the global
    %        reference. Anyway, it's better to work only with one context
    %        per process.
    stack.

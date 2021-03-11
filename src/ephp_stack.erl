-module(ephp_stack).

-author('manuel@altenwald.com').

-compile([{no_auto_import, [get/1]}]).

-behaviour(gen_server).

-include("ephp.hrl").

-export([start_link/0, destroy/0, get/0, get_array/0, get_array/1, push/6, pop/0]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3,
         terminate/2]).

start_link() ->
    case erlang:get(stack) of
        undefined ->
            %% FIXME: generate this information under a supervisor with
            %%        the context.
            {ok, PID} = gen_server:start_link(?MODULE, [self()], []),
            erlang:put(stack, PID),
            {ok, PID};
        PID ->
            case is_process_alive(PID) of
                true ->
                    {ok, PID};
                false ->
                    erlang:erase(stack),
                    start_link()
            end
    end.

destroy() ->
    case erlang:get(stack) of
        undefined ->
            ok;
        PID ->
            gen_server:stop(PID)
    end.

get() ->
    PID = erlang:get(stack),
    gen_server:call(PID, get).

get_array() ->
    get_array(0).

get_array(PopElements) ->
    PID = erlang:get(stack),
    gen_server:call(PID, {get_array, PopElements}).

push(_File, undefined, _Fun, _Args, _Class, _Object) ->
    ok;
push(File, {{line, Line}, _}, Fun, Args, Class, Object) ->
    PID = erlang:get(stack),
    Data = {push, File, Line, Fun, Args, Class, Object},
    gen_server:cast(PID, Data).

pop() ->
    PID = erlang:get(stack),
    gen_server:call(PID, pop).

%% gen_server callbacks

init([Parent]) ->
    monitor(process, Parent),
    {ok, []}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    erlang:erase(stack),
    ok.

handle_cast({push, File, Line, Fun, Args, Class, Object}, Stack) ->
    Type =
        if Class =:= undefined ->
               undefined;
           Object =:= undefined ->
               <<"::">>;
           true ->
               <<"->">>
        end,
    New = #stack_trace{function = Fun,
                       args = Args,
                       file = File,
                       line = Line,
                       object = Object,
                       class = Class,
                       type = Type},
    NewStack =
        case add_function(Fun) of
            [] when File =:= undefined ->
                Stack;
            _ ->
                [New | Stack]
        end,
    %% TODO: use filtering tables for tracing, implement ephp_tracer.
    case ephp_config:get_bool(<<"tracer.enable">>) of
        true ->
            gen_server:cast(ephp_tracer, New);
        false ->
            ok
    end,
    {noreply, NewStack}.

handle_call({get_array, PopElements}, _From, Stack) ->
    {_, GetStack} =
        lists:foldl(fun (StackEl, {0, Array}) ->
                            #stack_trace{function = Fun,
                                         file = File,
                                         line = Line,
                                         class = Class,
                                         object = Object,
                                         type = Type,
                                         args = Args} =
                                StackEl,
                            Element =
                                ephp_array:from_list([{<<"file">>, File}, {<<"line">>, Line}]
                                                     ++ add_function(Fun)
                                                     ++ add_class(Class)
                                                     ++ add_object(Object)
                                                     ++ add_type(Type)
                                                     ++ add_args(Fun, Args)),
                            {0, ephp_array:store(auto, Element, Array)};
                        (_Stack, {N, Array}) ->
                            {N - 1, Array}
                    end,
                    {PopElements, ephp_array:new()},
                    Stack),
    {reply, GetStack, Stack};
handle_call(get, _From, Stack) ->
    {reply, Stack, Stack};
handle_call(pop, _From, [] = Stack) ->
    {reply, undefined, Stack};
handle_call(pop, _From, [Head | Stack]) ->
    {reply, Head, Stack}.

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {stop, normal, State}.

%% internal functions

add_function(Fun) ->
    Incs = [<<"include">>, <<"include_once">>, <<"require">>, <<"require_once">>],
    case lists:member(
             ephp_string:to_lower(Fun), Incs)
    of
        true ->
            [];
        false ->
            [{<<"function">>, Fun}]
    end.

add_class(undefined) ->
    [];
add_class(Class) ->
    [{<<"class">>, Class}].

add_object(undefined) ->
    [];
add_object(Object) ->
    [{<<"object">>, Object}].

add_type(undefined) ->
    [];
add_type(Type) ->
    [{<<"type">>, Type}].

add_args(Fun, Args) ->
    [{<<"args">>, ephp_array:from_list(Args)}]
    ++ case add_function(Fun) of
           [] ->
               [{<<"function">>, Fun}];
           _ ->
               []
       end.

-module(ephp_func_misc).
-compile([warnings_as_errors]).

-export([
    init/1,
    define/3,
    sleep/2,
    usleep/2
]).

-include("ephp.hrl").

-spec init(Context :: context()) -> ok.

init(Context) ->
    Funcs = [
        define, sleep, usleep
    ],
    lists:foreach(fun(Func) ->
        Name = atom_to_binary(Func, utf8),
        ephp_context:register_func(Context, Name, ?MODULE, Func)  
    end, Funcs), 
    ok. 

-spec define(Context :: context(), Constant :: var_value(), 
    Content :: var_value()) -> boolean().

define(Context, {#constant{name=Constant},_}, {_UnParsedContent,Content}) ->
    ephp_context:register_const(Context, Constant, Content),
    true.

-spec sleep(Context :: context(), Seconds :: var_value()) -> false | integer().

sleep(_Context, {_, Seconds}) when is_number(Seconds) ->
    timer:sleep(trunc(Seconds) * 1000),
    0;

sleep(_Context, _) ->
    false.

-spec usleep(Context :: context(), MicroSeconds :: var_value()) -> 
    false | integer().

usleep(_Context, {_, MicroSeconds}) when is_number(MicroSeconds) ->
    timer:sleep(trunc(MicroSeconds) div 1000),
    0;

usleep(_Context, _) ->
    false.

%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------


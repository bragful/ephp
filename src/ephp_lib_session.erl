-module(ephp_lib_session).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    session_start/2
]).

-include("ephp.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    session_start
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [
    {<<"session.auto_start">>, <<"Off">>},
    {<<"session.cache_expire">>, 180},
    {<<"session.name">>, <<"PHPSESSID">>}
].

-spec session_start(context(), line()) -> ok.

session_start(Context, _Line) ->
    SessionArray = ephp_array:new(Module, Function),
    %% TODO: we have to get session ID
    %% TODO: we have to get session from backend
    ephp_context:set(Context, #variable{name = <<"_SESSION">>}, SessionArray),
    ok.

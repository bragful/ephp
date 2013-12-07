-module(ephp_func_date).
-compile([warnings_as_errors]).

-export([
    init/1,
    time/0
]).

-include("ephp.hrl").

-spec init(Context :: context()) -> ok.

init(Context) ->
    ephp_context:register_func(Context, <<"time">>, ?MODULE, time),
    ok. 

-spec time() -> integer().

time() ->
    {MS,S,_} = os:timestamp(),
    MS * 1000000 + S.

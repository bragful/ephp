-module(ephp_func_date).
-compile([export_all, warnings_as_errors]).

init(Context) ->
    ephp_context:register_func(Context, <<"time">>, ?MODULE, time),
    ok. 

time() ->
    {MS,S,_} = os:timestamp(),
    MS * 1000000 + S.

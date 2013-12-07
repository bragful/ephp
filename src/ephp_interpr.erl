-module(ephp_interpr).
-compile([export_all, warnings_as_errors]).

process(_Context, []) ->
    {ok, <<>>};

process(Context, Statements) ->
    {ok, lists:foldl(fun(Statement, Text) ->
        Result = run(Context, Statement),
        <<Text/binary, Result/binary>>
    end, <<>>, Statements)}.

run(_Context, {print_text, Text}) ->
    Text;

run(Context, {print, Expr}) ->
    Result = ephp_context:solve(Context, Expr),
    ephp_util:to_bin(Result);

run(Context, {eval, Statements}) ->
    lists:foldl(fun
        ({assign,_Var,_Expr}=Assign, GenText) ->
            ephp_context:solve(Context, Assign),
            GenText;
        ({if_block,Cond,TrueBlock,FalseBlock}, GenText) ->
            case ephp_context:solve(Context, Cond) of
            true ->
                Result = run(Context, {eval, TrueBlock}),
                <<GenText/binary, Result/binary>>;
            false ->
                Result = run(Context, {eval, FalseBlock}),
                <<GenText/binary, Result/binary>>
            end; 
        ({print_text, Text}, GenText) ->
            <<GenText/binary, Text/binary>>;
        ({print, Expr}, GenText) ->
            Result = ephp_context:solve(Context, Expr),
            ResText = ephp_util:to_bin(Result),
            <<GenText/binary, ResText/binary>>;
        ({call,Fun,Args}, GenText) ->
            {M,F,A} = ephp_context:call_func(Context, Fun, Args),
            Result = ephp_util:to_bin(erlang:apply(M,F,A)),
            <<GenText/binary, Result/binary>>;
        (Statement, _GenText) ->
            lager:error("unknown statement: ~p~n", [Statement]),
            throw(eunknownst)
    end, <<>>, Statements).

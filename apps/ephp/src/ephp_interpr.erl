-module(ephp_interpr).
-compile([export_all, warnings_as_errors]).

process(_Context, []) ->
    ok;

process(Context, [Statement|Statements]) ->
    run(Context, Statement),
    process(Context, Statements).

run(_Context, {print_text, Text}) ->
    io:format("~s", [Text]);

run(Context, {print, Expr}) ->
    Result = ephp_context:solve(Context, Expr),
    io:format("~s", [ephp_util:to_bin(Result)]);

run(Context, {eval, Statements}) ->
    lists:foreach(fun
        ({assign,_Var,_Expr}=Assign) ->
            ephp_context:solve(Context, Assign);
        ({if_block,Cond,TrueBlock,FalseBlock}) ->
            case ephp_context:solve(Context, Cond) of
            true ->
                run(Context, {eval, TrueBlock});
            false ->
                run(Context, {eval, FalseBlock})
            end; 
        ({print_text, Text}) ->
            io:format("~s", [Text]);
        ({print, Expr}) ->
            Result = ephp_context:solve(Context, Expr),
            io:format("~s", [ephp_util:to_bin(Result)]);
        (Statement) ->
            lager:error("unknown statement: ~p~n", [Statement]),
            throw(eunknownst)
    end, Statements).

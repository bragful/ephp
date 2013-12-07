-module(ephp_interpr).
-compile([export_all, warnings_as_errors]).

-include("ephp.hrl").

-spec process(Context :: context(), Statements :: [main_statement()]) -> 
    {ok, binary()}.

process(_Context, []) ->
    {ok, <<>>};

process(Context, Statements) ->
    {ok, lists:foldl(fun(Statement, Text) ->
        Result = run(Context, Statement),
        <<Text/binary, Result/binary>>
    end, <<>>, Statements)}.

-spec run(Context :: context(), Statements :: main_statement()) ->
    binary().

run(_Context, #print_text{text=Text}) ->
    Text;

run(Context, #print{expression=Expr}) ->
    Result = ephp_context:solve(Context, Expr),
    ephp_util:to_bin(Result);

run(Context, #eval{statements=Statements}) ->
    lists:foldl(fun
        (#assign{}=Assign, GenText) ->
            ephp_context:solve(Context, Assign),
            GenText;
        (#if_block{conditions=Cond}=IfBlock, GenText) ->
            case ephp_context:solve(Context, Cond) of
            true ->
                Result = run(Context, 
                    #eval{statements=IfBlock#if_block.true_block}),
                <<GenText/binary, Result/binary>>;
            false ->
                Result = run(Context, 
                    #eval{statements=IfBlock#if_block.false_block}),
                <<GenText/binary, Result/binary>>
            end; 
        (#print_text{text=Text}, GenText) ->
            <<GenText/binary, Text/binary>>;
        (#print{expression=Expr}, GenText) ->
            Result = ephp_context:solve(Context, Expr),
            ResText = ephp_util:to_bin(Result),
            <<GenText/binary, ResText/binary>>;
        (#call{name=Fun,args=Args}, GenText) ->
            {M,F,A} = ephp_context:call_func(Context, Fun, Args),
            Result = ephp_util:to_bin(erlang:apply(M,F,A)),
            <<GenText/binary, Result/binary>>;
        (Statement, _GenText) ->
            lager:error("unknown statement: ~p~n", [Statement]),
            throw(eunknownst)
    end, <<>>, Statements).

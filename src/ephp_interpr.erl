-module(ephp_interpr).
-compile([export_all, warnings_as_errors]).

-include("ephp.hrl").

-spec process(Context :: context(), Statements :: [main_statement()]) -> 
    {ok, binary()}.

process(_Context, []) ->
    {ok, <<>>};

process(Context, Statements) ->
    {ok, lists:foldl(fun(Statement, Text) ->
        {_Break, Result} = run(Context, Statement),
        <<Text/binary, Result/binary>>
    end, <<>>, Statements)}.

-spec run(Context :: context(), Statements :: main_statement()) ->
    {break | continue | return() | false, binary()}.

run(_Context, #print_text{text=Text}) ->
    {false, Text};

run(Context, #print{expression=Expr}) ->
    Result = ephp_context:solve(Context, Expr),
    {false, ephp_util:to_bin(Result)};

run(Context, #eval{statements=Statements}) ->
    lists:foldl(fun
        (#assign{}=Assign, {false, GenText}) ->
            ephp_context:solve(Context, Assign),
            {false, GenText};
        (#if_block{conditions=Cond}=IfBlock, {false, GenText}) ->
            case ephp_context:solve(Context, Cond) of
            true ->
                {Break, Result} = run(Context, 
                    #eval{statements=IfBlock#if_block.true_block}),
                {Break, <<GenText/binary, Result/binary>>};
            false when IfBlock#if_block.false_block =/= undefined ->
                {Break, Result} = run(Context, 
                    #eval{statements=IfBlock#if_block.false_block}),
                {Break, <<GenText/binary, Result/binary>>};
            _ ->
                {false, GenText}
            end; 
        (#for{init=Init,conditions=Cond,
                update=Update,loop_block=LB}, {false, GenText}) ->
            run(Context, #eval{statements=Init}),
            LoopBlock = if
                is_tuple(LB) -> [LB];
                is_list(LB) -> LB;
                is_atom(LB) -> [LB];
                LB =:= undefined -> []
            end,
            run_loop(pre, Context, Cond, LoopBlock ++ Update, GenText);
        (#foreach{kiter=Key,iter=Var,elements=RawElements,loop_block=LB}, {false, GenText}) ->
            LoopBlock = if
                is_tuple(LB) -> [LB];
                is_list(LB) -> LB;
                is_atom(LB) -> [LB];
                LB =:= undefined -> []
            end,
            Elements = ephp_context:get(Context, RawElements),
            run_foreach(Context, Key,Var,Elements,LoopBlock,GenText);
        (#while{type=Type,conditions=Cond,loop_block=LB}, {false, GenText}) ->
            LoopBlock = if
                is_tuple(LB) -> [LB];
                is_list(LB) -> LB;
                is_atom(LB) -> [LB];
                LB =:= undefined -> []
            end, 
            run_loop(Type, Context, Cond, LoopBlock, GenText);
        (#print_text{text=Text}, {false, GenText}) ->
            {false, <<GenText/binary, Text/binary>>};
        (#print{expression=Expr}, {false, GenText}) ->
            Result = ephp_context:solve(Context, Expr),
            Output = ephp_context:get_output(Context),
            ResText = ephp_util:to_bin(Result),
            {false, <<GenText/binary, Output/binary, ResText/binary>>};
        (#call{}=Call, {false, GenText}) ->
            ephp_context:solve(Context, Call), 
            Output = ephp_context:get_output(Context),
            {false, <<GenText/binary, Output/binary>>};
        ({Op, _Var}=MonoArith, {false, GenText}) when 
                Op =:= pre_incr orelse 
                Op =:= pre_decr orelse
                Op =:= post_incr orelse
                Op =:= post_decr ->
            ephp_context:solve(Context, MonoArith),
            {false, GenText};
        (break, {false, GenText}) ->
            {break, GenText};
        (continue, {false, GenText}) ->
            {continue, GenText};
        ({return,Value}, {false,GenText}) ->
            {{return,Value}, GenText};
        (_Statement, {false,_GenText}) ->
            throw(eunknownst);
        (_Statement, {Break, GenText}) ->
            {Break, GenText}
    end, {false, <<>>}, Statements).

-spec run_loop(
    PrePost :: (pre | post),
    Context :: context(),
    Cond :: condition(),
    Statements :: [statement()],
    GenText :: binary()) -> {break | continue | return() | false, binary()}.

run_loop(PrePost, Context, Cond, Statements, GenText) ->
    case PrePost =:= post orelse ephp_context:solve(Context, Cond) of
    true -> 
        {Break, ResText} = run(Context, #eval{statements=Statements}),
        NewGenText = <<GenText/binary, ResText/binary>>,
        case Break =/= break andalso not is_tuple(Break) andalso ephp_context:solve(Context, Cond) of
        true ->
            run_loop(PrePost, Context, Cond, Statements, NewGenText);
        false ->
            {case Break of {return,Ret} -> {return,Ret}; _ -> false end, NewGenText}
        end;
    false ->
        {false, GenText}
    end.

-spec run_foreach(
    Context :: context(),
    Key :: variable(),
    Var :: variable(),
    Elements :: mixed(),
    Statements :: [statement()],
    GenText :: binary()) -> binary().

run_foreach(_Context, _Key, _Var, [], _Statements, GenText) ->
    {false, GenText};

run_foreach(Context, Key, Var, [{KeyVal,VarVal}|Elements], Statements, GenText) ->
    case Key of 
        undefined -> ok;
        _ -> ephp_context:set(Context, Key, KeyVal)
    end,
    ephp_context:set(Context, Var, VarVal),
    {Break, NewText} = run(Context, #eval{statements=Statements}),
    NewGenText = <<GenText/binary, NewText/binary>>,
    if 
        Break =/= break andalso not is_tuple(Break) ->
            run_foreach(Context, Key, Var, Elements, Statements, NewGenText);
        true ->
            {case Break of {return,Ret} -> {return,Ret}; _ -> false end, NewGenText}
    end.

-module(ephp_interpr).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([
    process/2,
    run/2
]).

-include("ephp.hrl").

-spec process(Context :: context(), Statements :: [main_statement()]) ->
    {ok, binary() | return() | false}.

process(_Context, []) ->
    {ok, <<>>};

process(Context, Statements) ->
    Value = lists:foldl(fun
        (Statement, false) ->
            run(Context, Statement);
        (_Statement, Return) ->
            Return
    end, false, Statements),
    {ok, Value}.

-type break() :: break | {break, pos_integer()}.

-type flow_status() :: break() | continue | return() | false.

-spec run(context(), main_statement()) -> flow_status().

run(Context, #print_text{text=Text}) ->
    ephp_context:set_output(Context, Text),
    false;

run(Context, #print{expression=Expr, line=Line}) ->
    Result = ephp_context:solve(Context, Expr),
    ephp_context:set_output(Context, ephp_data:to_bin(Context, Line, Result)),
    false;

run(Context, #eval{statements=Statements}) ->
    lists:foldl(fun(Statement, State) ->
        run_depth(Context, Statement, State)
    end, false, Statements).

-spec run_depth(context(), statement(), flow_status()) -> flow_status().

run_depth(Context, #eval{}=Eval, false) ->
    run(Context, Eval);

run_depth(Context, #assign{}=Assign, Return) ->
    ephp_context:solve(Context, Assign),
    Return;

run_depth(Context, #if_block{conditions=Cond}=IfBlock, false) ->
    #if_block{true_block = TrueBlock, false_block = FalseBlock} = IfBlock,
    case ephp_data:to_boolean(ephp_context:solve(Context, Cond)) of
        true when is_list(TrueBlock) ->
            run(Context, #eval{statements = TrueBlock});
        true ->
            run(Context, #eval{statements = [TrueBlock]});
        false when is_list(FalseBlock) ->
            run(Context, #eval{statements = FalseBlock});
        false when FalseBlock =:= undefined ->
            false;
        false ->
            run(Context, #eval{statements = [FalseBlock]})
    end;

run_depth(Context, #switch{condition=Cond, cases=Cases}, false) ->
    case run_switch(Context, Cond, Cases) of
        {seek, false} ->
            {_, Return} = run_switch(Context, default, Cases),
            Return;
        {_, Return} ->
            Return
    end,
    case Return of
        false -> false;
        {return, R} -> {return, R};
        break -> false;
        {break, 0} -> false;
        {break, N} -> {break, N-1}
    end;

run_depth(Context, #for{init=Init,conditions=Cond,
        update=Update,loop_block=LB}, false) ->
    run(Context, #eval{statements=Init}),
    LoopBlock = if
        is_tuple(LB) -> [LB];
        is_list(LB) -> LB;
        LB =:= undefined -> [];
        is_atom(LB) -> [LB]
    end,
    run_loop(pre, Context, Cond, LoopBlock ++ Update);

run_depth(Context, #foreach{
        kiter=Key, iter=Var, elements=RawElements, loop_block=LB}=FE, false) ->
    LoopBlock = if
        is_tuple(LB) -> [LB];
        is_list(LB) -> LB;
        LB =:= undefined -> [];
        is_atom(LB) -> [LB]
    end,
    case ephp_context:solve(Context, RawElements) of
        ProcElements when ?IS_ARRAY(ProcElements) ->
            Elements = ephp_array:to_list(ProcElements),
            run_foreach(Context, Key, Var, Elements, LoopBlock);
        _ ->
            Line = FE#foreach.line,
            File = ephp_context:get_active_file(Context),
            Data = {<<"foreach">>},
            Error = {error, eargsupplied, Line, File, ?E_WARNING, Data},
            ephp_error:handle_error(Context, Error),
            false
    end;

run_depth(Context, #while{type=Type,conditions=Cond,loop_block=LB}, false) ->
    LoopBlock = if
        is_tuple(LB) -> [LB];
        is_list(LB) -> LB;
        LB =:= undefined -> [];
        is_atom(LB) -> [LB]
    end,
    run_loop(Type, Context, Cond, LoopBlock);

run_depth(Context, #print_text{text=Text}, false) ->
    ephp_context:set_output(Context, Text),
    false;

run_depth(Context, #print{expression=Expr, line=Line}, false) ->
    Result = ephp_context:solve(Context, Expr),
    ResText = ephp_data:to_bin(Context, Line, Result),
    ephp_context:set_output(Context, ResText),
    false;

run_depth(Context, #call{}=Call, false) ->
    ephp_func:run(Context, Call);

run_depth(Context, {Op, _Var, _Line}=MonoArith, false) when
        Op =:= pre_incr orelse
        Op =:= pre_decr orelse
        Op =:= post_incr orelse
        Op =:= post_decr ->
    ephp_context:solve(Context, MonoArith),
    false;

run_depth(Context, #operation{}=Op, false) ->
    ephp_context:solve(Context, Op),
    false;

run_depth(Context, #class{}=Class, Return) ->
    ephp_context:register_class(Context, Class),
    Return;

run_depth(Context, #function{name=Name, args=Args, code=Code}, Return) ->
    ephp_context:register_func(Context, Name, Args, Code, undefined),
    Return;

run_depth(Context, {global, GlobalVar, Line}, Return) ->
    ephp_context:solve(Context, {global, GlobalVar, Line}),
    Return;

run_depth(_Context, break, false) ->
    break;

run_depth(_Context, continue, false) ->
    continue;

run_depth(Context, {return,Value,_Line}, false) ->
    {return, ephp_context:solve(Context, Value)};

run_depth(_Context, {return,Value}, false) ->
    {return, Value};

run_depth(_Context, {break, N}, false) ->
    {break, N-1};

run_depth(_Context, Boolean, false) when is_boolean(Boolean) ->
    false;

run_depth(_Context, #int{}, false) ->
    false;

run_depth(_Context, #float{}, false) ->
    false;

run_depth(_Context, #text{}, false) ->
    false;

run_depth(Context, #text_to_process{}=TP, false) ->
    ephp_context:solve(Context, TP),
    false;

run_depth(Context, #variable{idx=[{object,#call{},_}]}=Var, false) ->
    ephp_context:solve(Context, Var),
    false;

run_depth(Context, #constant{type=define,name=Name,value=Expr}, false) ->
    Value = ephp_context:solve(Context, Expr),
    ephp_context:register_const(Context, Name, Value),
    false;

run_depth(_Context, #constant{}, false) ->
    false;

run_depth(Context, {silent, Statement}, false) ->
    ephp_error:run_quiet(ephp_context:get_errors_id(Context), fun() ->
        run_depth(Context, Statement, false)
    end);

run_depth(_Context, die, false) ->
    throw(die);

run_depth(_Context, Statement, false) ->
    ephp_error:error({error, eunknownst, undefined, ?E_CORE_ERROR, Statement}),
    break;

run_depth(_Context, _Statement, Break) ->
    Break.

exit_cond({return, Ret}) -> {return, Ret};
exit_cond({break, 0}) -> false;
exit_cond({break, N}) -> {break, N-1};
exit_cond(false) -> false.

-spec run_loop(
    PrePost :: (pre | post),
    Context :: context(),
    Cond :: condition(),
    Statements :: [statement()]) ->
        break() | continue | return() | false.

run_loop(pre, Context, Cond, Statements) ->
    case ephp_data:to_bool(ephp_context:solve(Context, Cond)) of
        true ->
            case run(Context, #eval{statements=Statements}) of
                false ->
                    run_loop(pre, Context, Cond, Statements);
                Return ->
                    exit_cond(Return)
            end;
        false ->
            false
    end;

run_loop(post, Context, Cond, Statements) ->
    case run(Context, #eval{statements=Statements}) of
        false ->
            case ephp_data:to_bool(ephp_context:solve(Context, Cond)) of
                true ->
                    run_loop(post, Context, Cond, Statements);
                false ->
                    false
            end;
        Return ->
            exit_cond(Return)
    end.

-spec run_foreach(
    Context :: context(),
    Key :: variable(),
    Var :: variable(),
    Elements :: mixed(),
    Statements :: [statement()]) -> break() | return() | false.

run_foreach(_Context, _Key, _Var, [], _Statements) ->
    false;

run_foreach(Context, Key, Var, [{KeyVal,VarVal}|Elements], Statements) ->
    case Key of
        undefined -> ok;
        _ -> ephp_context:set(Context, Key, KeyVal)
    end,
    ephp_context:set(Context, Var, VarVal),
    Break = run(Context, #eval{statements=Statements}),
    if
        Break =/= break andalso not is_tuple(Break) ->
            run_foreach(Context, Key, Var, Elements, Statements);
        true ->
            case Break of
                {return,Ret} -> {return,Ret};
                {break,0} -> false;
                {break,N} -> {break,N-1};
                _ -> false
            end
    end.

-type switch_flow() :: seek | run | exit.

-spec run_switch(context(), condition() | default, [switch_case()]) ->
    {switch_flow(), break() | return() | false}.

run_switch(Context, default, Cases) ->
    lists:foldl(fun
        (_SwitchCase, {exit, Return}) ->
            {exit, Return};
        (#switch_case{label=default, code_block=Code}, {seek, false}) ->
            case run(Context, #eval{statements=Code}) of
                break -> {exit, false};
                {break, 0} -> {exit, false};
                {break, N} -> {exit, {break, N-1}};
                {return, R} -> {exit, {return, R}};
                false -> {run, false}
            end;
        (_Case, {seek, false}) ->
            {seek, false};
        (Case, {run, false}) ->
            Break = run(Context,
                #eval{statements=Case#switch_case.code_block}),
            case Break of
                break -> {exit, false};
                {break, 0} -> {exit, false};
                {break, N} -> {exit, {break, N-1}};
                {return, R} -> {exit, {return, R}};
                false -> {run, false}
            end
    end, {seek, false}, Cases);


run_switch(Context, Cond, Cases) ->
    MatchValue = ephp_context:solve(Context, Cond),
    lists:foldl(fun
        (_SwitchCase, {exit, Return}) ->
            {exit, Return};
        (Case, {run, false}) ->
            Break = run(Context,
                #eval{statements=Case#switch_case.code_block}),
            case Break of
                break -> {exit, false};
                {break, 0} -> {exit, false};
                {break, N} -> {exit, {break, N-1}};
                {return, R} -> {exit, {return, R}};
                false -> {run, false}
            end;
        (#switch_case{label=default}, {seek, false}) ->
            {seek, false};
        (#switch_case{label=LabelValue}=Case, {seek, false}) ->
            Op = #operation{
                type = <<"==">>,
                expression_left=MatchValue,
                expression_right=LabelValue},
            case ephp_context:solve(Context, Op) of
            true ->
                Break = run(Context,
                    #eval{statements=Case#switch_case.code_block}),
                case Break of
                    break -> {exit, false};
                    {break, 0} -> {exit, false};
                    {break, N} -> {exit, {break, N-1}};
                    {return, R} -> {exit, {return, R}};
                    false -> {run, false}
                end;
            false ->
                {seek, false}
            end
    end, {seek, false}, Cases).

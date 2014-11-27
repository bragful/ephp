-module(ephp_interpr).

-export([
    process/2,
    run/2
]).

-include("ephp.hrl").

-spec process(Context :: context(), Statements :: [main_statement()]) -> 
    {ok, binary()}.

process(_Context, []) ->
    {ok, <<>>};

process(Context, Statements) ->
    lists:foreach(fun(Statement) ->
        run(Context, Statement)
    end, Statements),
    {ok, ephp_context:get_output(Context)}.

-spec run(Context :: context(), Statements :: main_statement()) ->
    break | continue | return() | false.

run(Context, #print_text{text=Text}) ->
    ephp_context:set_output(Context, Text),
    false;

run(Context, #print{expression=Expr}) ->
    Result = ephp_context:solve(Context, Expr),
    ephp_context:set_output(Context, ephp_util:to_bin(Result)),
    false;

run(Context, #eval{statements=Statements}) ->
    lists:foldl(fun
        (#eval{}=Eval, false) ->
            run(Context, Eval);
        (#assign{}=Assign, Return) ->
            ephp_context:solve(Context, Assign),
            Return;
        (#if_block{conditions=Cond}=IfBlock, false) ->
            case ephp_context:solve(Context, Cond) of
            true ->
                Break = run(Context, 
                    #eval{statements=IfBlock#if_block.true_block}),
                Break;
            false when IfBlock#if_block.false_block =/= undefined ->
                Break = run(Context, 
                    #eval{statements=IfBlock#if_block.false_block}),
                Break;
            _ ->
                false
            end;
        (#for{init=Init,conditions=Cond,
                update=Update,loop_block=LB}, false) ->
            run(Context, #eval{statements=Init}),
            LoopBlock = if
                is_tuple(LB) -> [LB];
                is_list(LB) -> LB;
                is_atom(LB) -> [LB];
                LB =:= undefined -> []
            end,
            run_loop(pre, Context, Cond, LoopBlock ++ Update);
        (#foreach{kiter=Key,iter=Var,elements=RawElements,loop_block=LB}, false) ->
            LoopBlock = if
                is_tuple(LB) -> [LB];
                is_list(LB) -> LB;
                is_atom(LB) -> [LB];
                LB =:= undefined -> []
            end,
            Elements = ?DICT:to_list(ephp_context:get(Context, RawElements)),
            run_foreach(Context, Key,Var,Elements,LoopBlock);
        (#while{type=Type,conditions=Cond,loop_block=LB}, false) ->
            LoopBlock = if
                is_tuple(LB) -> [LB];
                is_list(LB) -> LB;
                is_atom(LB) -> [LB];
                LB =:= undefined -> []
            end, 
            run_loop(Type, Context, Cond, LoopBlock);
        (#print_text{text=Text}, false) ->
            ephp_context:set_output(Context, Text),
            false;
        (#print{expression=Expr}, false) ->
            Result = ephp_context:solve(Context, Expr),
            ResText = ephp_util:to_bin(Result),
            ephp_context:set_output(Context, ResText),
            false;
        (#call{line=Line}=Call, false) ->
            try 
                ephp_context:solve(Context, Call),
                false
            catch
                throw:die ->
                    {return, null, Line};
                throw:{error, eundefun, _, Fun} ->
                    %% TODO: format better the output errors
                    File = ephp_context:get_const(Context, <<"__FILE__">>),
                    Error = io_lib:format(
                        "~nFatal error: Call to undefined function ~s()"
                        " in ~s on line ~p~n",
                        [Fun, File, ephp_util:get_line(Line)]),
                    ephp_context:set_output(Context, Error),
                    {return, null, Line}
            end;
        ({Op, _Var, _Line}=MonoArith, false) when 
                Op =:= pre_incr orelse 
                Op =:= pre_decr orelse
                Op =:= post_incr orelse
                Op =:= post_decr ->
            ephp_context:solve(Context, MonoArith),
            false;
        (#operation{}=Op, false) ->
            ephp_context:solve(Context, Op),
            false;
        (#function{name=Name, args=Args, code=Code}, Return) ->
            ephp_context:register_func(Context, Name, Args, Code),
            Return;
        ({global, GlobalVar, Line}, Return) ->
            ephp_context:solve(Context, {global, GlobalVar, Line}),
            Return;
        (break, false) ->
            break;
        (continue, false) ->
            continue;
        ({return,Value,Line}, false) ->
            {return,Value,Line};
        (_Statement, false) ->
            %% TODO: do better error handling
            io:format("FATAL: ~p~n", [_Statement]),
            throw(eunknownst);
        (_Statement, Break) ->
            Break
    end, false, Statements).

-spec run_loop(
    PrePost :: (pre | post),
    Context :: context(),
    Cond :: condition(),
    Statements :: [statement()]) ->
        {break | continue | return() | false, binary()}.

run_loop(PrePost, Context, Cond, Statements) ->
    case PrePost =:= post orelse ephp_context:solve(Context, Cond) of
    true -> 
        Break = run(Context, #eval{statements=Statements}),
        case Break =/= break andalso not is_tuple(Break) andalso ephp_context:solve(Context, Cond) of
        true ->
            run_loop(PrePost, Context, Cond, Statements);
        false ->
            case Break of 
                {return,Ret,Line} -> {return,Ret,Line};
                _ -> false 
            end
        end;
    false ->
        false
    end.

-spec run_foreach(
    Context :: context(),
    Key :: variable(),
    Var :: variable(),
    Elements :: mixed(),
    Statements :: [statement()]) -> binary().

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
                {return,Ret,Line} -> {return,Ret,Line};
                _ -> false 
            end
    end.

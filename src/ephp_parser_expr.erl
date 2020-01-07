-module(ephp_parser_expr).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([expression/3, add_op/2, precedence/1]).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-import(ephp_parser, [
    add_pos/2, new_line/1, copy_rowcol/2, add_line/2, remove_spaces/2,
    throw_error/3, inc_pos/1, get_line/1,

    array_def_level/1, code_statement_level/1, arg_level/1,
    enclosed_level/1,

    variable/3, comment_line/3, comment_block/3, constant/3
]).

array_def_54_level(Parser) -> Parser#parser{level = array_def, array_type = php54}.

add_op('end', []) ->
    [];
add_op('end', [{op, [#constant{name = Name, line = RowCol} = Constant]}]) ->
    Op = case Name of
        <<I:8,N:8,T:8>> when ?OR(I,$i,$I) andalso ?OR(N,$n,$N) andalso
                             ?OR(T,$t,$T) ->
            OpL = <<"(int)">>,
            {OpL, precedence(OpL), RowCol};
        <<I:8,N:8,T:8,E:8,G:8,E:8,R:8>> when ?OR(I,$i,$I) andalso ?OR(N,$n,$N)
                                     andalso ?OR(T,$t,$T) andalso ?OR(E,$e,$E)
                                     andalso ?OR(G,$g,$G) andalso ?OR(R,$r,$R) ->
            OpL = <<"(int)">>,
            {OpL, precedence(OpL), RowCol};
        <<F:8,L:8,O:8,A:8,T:8>> when ?OR(F,$F,$f) andalso ?OR(L,$L,$l) andalso
                                     ?OR(O,$O,$o) andalso ?OR(A,$A,$a) andalso
                                     ?OR(T,$T,$t) ->
            OpL = <<"(float)">>,
            {OpL, precedence(OpL), RowCol};
        <<D:8,O:8,U:8,B:8,L:8,E:8>> when ?OR(D,$D,$d) andalso ?OR(O,$O,$o)
                                 andalso ?OR(U,$U,$u) andalso ?OR(B,$B,$b)
                                 andalso ?OR(L,$L,$l) andalso ?OR(E,$E,$e) ->
            OpL = <<"(float)">>,
            {OpL, precedence(OpL), RowCol};
        <<S:8,T:8,R:8,I:8,N:8,G:8>> when ?OR(S,$S,$s) andalso
                                         ?OR(T,$T,$t) andalso
                                         ?OR(R,$R,$r) andalso
                                         ?OR(I,$I,$i) andalso
                                         ?OR(N,$N,$n) andalso
                                         ?OR(G,$G,$g) ->
            OpL = <<"(string)">>,
            {OpL, precedence(OpL), RowCol};
        % array will be captured in the array() part
        <<O:8,B:8,J:8,E:8,C:8,T:8>> when ?OR(O,$O,$o) andalso
                                         ?OR(B,$B,$b) andalso
                                         ?OR(J,$J,$j) andalso
                                         ?OR(E,$E,$e) andalso
                                         ?OR(C,$C,$c) andalso
                                         ?OR(T,$T,$t) ->
            OpL = <<"(object)">>,
            {OpL, precedence(OpL), RowCol};
        <<B:8,O:8,O:8,L:8,E:8,A:8,N:8>> when ?OR(B,$B,$b) andalso
                                             ?OR(O,$O,$o) andalso
                                             ?OR(L,$L,$l) andalso
                                             ?OR(E,$E,$e) andalso
                                             ?OR(A,$A,$a) andalso
                                             ?OR(N,$N,$n) ->
            OpL = <<"(bool)">>,
            {OpL, precedence(OpL), RowCol};
        <<B:8,O:8,O:8,L:8>> when ?OR(B,$B,$b) andalso ?OR(O,$O,$o) andalso
                                 ?OR(L,$L,$l) ->
            OpL = <<"(bool)">>,
            {OpL, precedence(OpL), RowCol};
        <<U:8,N:8,S:8,E:8,T:8>> when ?OR(U,$U,$u) andalso ?OR(N,$N,$n) andalso
                                     ?OR(S,$S,$s) andalso ?OR(E,$E,$e) andalso
                                     ?OR(T,$T,$t) ->
            OpL = <<"(unset)">>,
            {OpL, precedence(OpL), RowCol};
        _ ->
            Constant
    end,
    solve([Op]);
add_op('end', [{op, Content}]) ->
    solve(process_incr_decr(Content));
add_op(Add, [{op, Content}|Parsed]) when is_list(Add) ->
    [{op, Content ++ Add}|Parsed];
add_op(Add, [{op, Content}|Parsed]) ->
    [{op, Content ++ [Add]}|Parsed];
add_op(Add, Parsed) ->
    [{op,[Add]}|Parsed].

process_incr_decr(Content) ->
    process_incr_decr(Content, []).

process_incr_decr([], Processed) ->
    Processed;
process_incr_decr([{<<"++">>, _, RowCol}, #variable{} = V|Rest], Processed) ->
    process_incr_decr(Rest, Processed ++ [{pre_incr, V, RowCol}]);
process_incr_decr([{<<"--">>, _, RowCol}, #variable{} = V|Rest], Processed) ->
    process_incr_decr(Rest, Processed ++ [{pre_decr, V, RowCol}]);
process_incr_decr([#variable{} = V, {<<"++">>, _, RowCol}|Rest], Processed) ->
    process_incr_decr(Rest, Processed ++ [{post_incr, V, RowCol}]);
process_incr_decr([#variable{} = V,{<<"--">>, _, RowCol}|Rest], Processed) ->
    process_incr_decr(Rest, Processed ++ [{post_decr, V, RowCol}]);
process_incr_decr([A|Rest], Processed) ->
    process_incr_decr(Rest, Processed ++ [A]).

number(<<"0", X:8, Rest/binary>>, Parser, []) when ?OR(X, $X, $x) ->
    hexa(Rest, add_pos(Parser, 2), []);
number(<<"0", N:8, Rest/binary>>, Parser, []) when ?IS_OCT(N) ->
    octa(<<N:8, Rest/binary>>, add_pos(Parser, 2), []);
number(<<A:8, Rest/binary>>, Parser, []) when ?IS_NUMBER(A) orelse A =:= $- ->
    number(Rest, inc_pos(Parser), [add_line(#int{int = <<A:8>>}, Parser)]);
number(<<A:8, Rest/binary>>, Parser, [#int{int = N} = I]) when ?IS_NUMBER(A) ->
    number(Rest, inc_pos(Parser), [I#int{int = <<N/binary,A:8>>}]);
number(<<".", Rest/binary>>, Parser, []) ->
    number(Rest, inc_pos(Parser), [add_line(#float{float = <<"0.">>}, Parser)]);
number(<<".", Rest/binary>>, Parser, [#int{int = N, line = Line}]) ->
    number(Rest, inc_pos(Parser), [#float{float = <<N/binary,".">>, line = Line}]);
number(<<A:8, Rest/binary>>, Parser, [#float{float = N} = F]) when ?IS_NUMBER(A) ->
    number(Rest, inc_pos(Parser), [F#float{float = <<N/binary, A:8>>}]);
number(Rest, Parser, [#int{int = N} = I]) ->
    {Rest, Parser, [I#int{int = binary_to_integer(N)}]};
number(Rest, Parser, [#float{float = N} = F]) ->
    {Rest, Parser, [F#float{float = binary_to_float(N)}]}.

hexa(<<A:8,Rest/binary>>, Parser, []) when ?IS_HEX(A) ->
    hexa(Rest, inc_pos(Parser), [add_line(#int{int = <<A:8>>}, Parser)]);
hexa(<<A:8,Rest/binary>>, Parser, [#int{int=N}=I]) when ?IS_HEX(A) ->
    hexa(Rest, inc_pos(Parser), [I#int{int = <<N/binary, A:8>>}]);
hexa(Rest, Parser, [#int{int = N} = I]) ->
    {Rest, Parser, [I#int{int = binary_to_integer(N, 16)}]}.

octa(<<A:8,Rest/binary>>, Parser, []) when ?IS_OCT(A) ->
    octa(Rest, inc_pos(Parser), [add_line(#int{int = <<A:8>>}, Parser)]);
octa(<<A:8,Rest/binary>>, Parser, [#int{int=N}=I]) when ?IS_OCT(A) ->
    octa(Rest, inc_pos(Parser), [I#int{int = <<N/binary, A:8>>}]);
octa(Rest, Parser, [#int{int = N} = I]) ->
    {Rest, Parser, [I#int{int = binary_to_integer(N, 8)}]}.

array_def(<<SP:8,Rest/binary>>, Parser, Args) when ?IS_SPACE(SP) ->
    array_def(Rest, inc_pos(Parser), Args);
array_def(<<SP:8,Rest/binary>>, Parser, Args) when ?IS_NEWLINE(SP) ->
    array_def(Rest, new_line(Parser), Args);
array_def(<<")",Rest/binary>>, #parser{level = array_def, array_type = old} = Parser, Args) ->
    {Rest, inc_pos(Parser), Args};
array_def(<<"]",Rest/binary>>, #parser{level = array_def, array_type = php54} = Parser, Args) ->
    {Rest, inc_pos(Parser), Args};
%% TODO add error missing closing params
array_def(Rest, Parser, Args) when Rest =/= <<>> ->
    case expression(Rest, Parser, []) of
        {<<")",Rest0/binary>>, #parser{level = array_def, array_type = old} = Parser0, [Idx,Arg]} ->
            NewArg = add_line(#array_element{idx = Idx, element = Arg}, Parser),
            {Rest0, inc_pos(Parser0), Args ++ [NewArg]};
        {<<")",Rest0/binary>>, #parser{level = array_def, array_type = old} = Parser0, undefined} ->
            {Rest0, inc_pos(Parser0), Args};
        {<<")",Rest0/binary>>, #parser{level = array_def, array_type = old} = Parser0, Arg} ->
            NewArg = add_line(#array_element{element = Arg}, Parser),
            {Rest0, inc_pos(Parser0), Args ++ [NewArg]};
        {<<"]",Rest0/binary>>, #parser{level = array_def, array_type = php54} = Parser0, [Idx,Arg]} ->
            NewArg = add_line(#array_element{idx = Idx, element = Arg}, Parser),
            {Rest0, inc_pos(Parser0), Args ++ [NewArg]};
        {<<"]",Rest0/binary>>, #parser{level = array_def, array_type = php54} = Parser0, undefined} ->
            {Rest0, inc_pos(Parser0), Args};
        {<<"]",Rest0/binary>>, #parser{level = array_def, array_type = php54} = Parser0, Arg} ->
            NewArg = add_line(#array_element{element = Arg}, Parser),
            {Rest0, inc_pos(Parser0), Args ++ [NewArg]};
        {<<",",Rest0/binary>>, Parser0, [Idx,Arg]} ->
            NewArg = add_line(#array_element{idx = Idx, element = Arg}, Parser),
            array_def(Rest0, inc_pos(Parser0), Args ++ [NewArg]);
        {<<",",Rest0/binary>>, Parser0, Arg} ->
            NewArg = add_line(#array_element{element = Arg}, Parser),
            array_def(Rest0, inc_pos(Parser0), Args ++ [NewArg])
    end.

-spec expression(Text :: binary(),
                 parser(),
                 [expression()]) -> {binary(), parser(), [expression()] | expression()}.

% CONSTANT / FUNCTION when -> is used
expression(<<A:8,_/binary>> = Rest, Parser,
           [{op, [_, {<<"->">>, _, _}]}|_] = Parsed) when
        ?IS_ALPHA(A) orelse A =:= $_ ->
    {Rest0, Parser0, [Constant]} = constant(Rest, Parser, []),
    expression(Rest0, copy_rowcol(Parser0, Parser), add_op(Constant, Parsed));
% ARRAY(...) -old-
expression(<<A:8,R:8,R:8,A:8,Y:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(A,$a,$A) andalso ?OR(R,$r,$R) andalso ?OR(Y,$y,$Y)
        andalso not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_) ->
    case remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 5)) of
        {<<")", _/binary>> = Rest0, Parser0} ->
            %% (array) cast
            OpL = <<"(array)">>,
            NewParsed = add_op({OpL, precedence(OpL), Parser}, Parsed),
            expression(Rest0, Parser0, NewParsed);
        {<<"(", Rest0/binary>>, Parser0} ->
            NewParser = array_def_level(inc_pos(Parser0)),
            {Rest1, Parser1, Content} = array_def(Rest0, NewParser, []),
            NewParsed = add_op(add_line(#array{elements = Content}, Parser), Parsed),
            expression(Rest1, copy_rowcol(Parser1, Parser), NewParsed)
    end;
% [...] -array new-
expression(<<"[", Rest/binary>>, Parser, []) ->
    NewParser = array_def_54_level(inc_pos(Parser)),
    {Rest1, Parser1, Content} = array_def(Rest, NewParser, []),
    NewParsed = add_op(add_line(#array{elements=Content}, Parser), []),
    expression(Rest1, copy_rowcol(Parser1, Parser), NewParsed);
expression(<<"[", Rest/binary>>, Parser, [{op, []}|_] = Parsed) ->
    % ARRAY DEF
    NewParser = array_def_54_level(inc_pos(Parser)),
    {Rest1, Parser1, Content} = array_def(Rest, NewParser, []),
    NewParsed = add_op(add_line(#array{elements = Content}, Parser), Parsed),
    expression(Rest1, copy_rowcol(Parser1, Parser), NewParsed);
expression(<<"[", Rest/binary>>, Parser, [{op, Op}|_] = Parsed) ->
    case lists:last(Op) of
        {_, {RightOrLeft, _}, _} when RightOrLeft =:= right orelse
                                      RightOrLeft =:= left ->
            % ARRAY DEF
            NewParser = array_def_54_level(inc_pos(Parser)),
            {Rest1, Parser1, Content} = array_def(Rest, NewParser, []),
            NewParsed = add_op(add_line(#array{elements = Content}, Parser), Parsed),
            expression(Rest1, copy_rowcol(Parser1, Parser), NewParsed);
        _ ->
            % ARRAY INDEX
            NewParsed = [#variable{name = add_op('end', Parsed)}],
            {Rest0, Parser0, [Parsed0]} =
                variable(<<"[",Rest/binary>>, Parser, NewParsed),
            expression(Rest0, Parser0, add_op(Parsed0, []))
    end;
% NULL
expression(<<N:8,U:8,L:8,L:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(N,$N,$n) andalso ?OR(U,$U,$u) andalso ?OR(L,$L,$l)
        andalso not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_) ->
    expression(<<SP:8, Rest/binary>>, add_pos(Parser, 4), add_op(undefined, Parsed));
% TRUE
expression(<<T:8,R:8,U:8,E:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(T,$t,$T) andalso ?OR(R,$r,$R) andalso ?OR(U,$u,$U)
        andalso ?OR(E,$e,$E)
        andalso not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_) ->
    expression(<<SP:8, Rest/binary>>, add_pos(Parser, 4), add_op(true, Parsed));
% FALSE
expression(<<F:8,A:8,L:8,S:8,E:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(F,$f,$F) andalso ?OR(A,$a,$A) andalso ?OR(L,$l,$L)
        andalso ?OR(S,$s,$S) andalso ?OR(E,$e,$E)
        andalso not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_) ->
    expression(<<SP:8, Rest/binary>>, add_pos(Parser, 5), add_op(false, Parsed));
% FUNCTION(...) -closure-
expression(<<F:8,U:8,N:8,C:8,T:8,I:8,O:8,N:8,SP:8,Rest/binary>>,
           Parser, Parsed) when
        ?OR(F,$F,$f) andalso ?OR(U,$U,$u) andalso ?OR(N,$N,$n) andalso
        ?OR(C,$C,$c) andalso ?OR(T,$T,$t) andalso ?OR(I,$I,$i) andalso
        ?OR(O,$O,$o) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $() ->
    {<<"(",Rest0/binary>>, Parser0} =
        remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 9)),
    {Rest1, Parser1, Args} = ephp_parser_func:funct_args(Rest0, Parser0, []),
    BaseFunction = add_line(#function{args = Args}, Parser),
    {Rest2, Parser2, Function} = ephp_parser_func:st_use_or_block(Rest1, Parser1, BaseFunction),
    expression(Rest2, copy_rowcol(Parser2, Parser), add_op(Function, Parsed));
% INSTANCEOF
expression(<<I:8,N:8,S:8,T:8,A:8,N:8,C:8,E:8,O:8,F:8,SP:8,Rest/binary>>,
           Parser, Parsed) when
        ?OR(I,$I,$i) andalso ?OR(N,$N,$n) andalso ?OR(S,$S,$s) andalso
        ?OR(T,$T,$t) andalso ?OR(A,$A,$a) andalso ?OR(C,$C,$c) andalso
        ?OR(E,$E,$e) andalso ?OR(O,$O,$o) andalso ?OR(F,$F,$f) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8, Rest/binary>>, Parser),
    OpL = <<"instanceof">>,
    expression(Rest0, Parser0, add_op({instanceof, precedence(OpL), get_line(Parser)}, Parsed));
% NEW ...
expression(<<N:8,E:8,W:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(N,$N,$n) andalso ?OR(E,$E,$e) andalso ?OR(W,$W,$w) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $() ->
    {Rest1, Parser1, {NS, ObjName}} = case remove_spaces(<<SP:8,Rest/binary>>, Parser) of
        {<<"$", _/binary>> = Rest0, Parser0} ->
            {RestEx, ParserEx, Exp} = expression(Rest0, Parser0, []),
            {RestEx, ParserEx, {Parser#parser.namespace, Exp}};
        {<<A:8, _/binary>> = Rest0, Parser0} when
                ?IS_ALPHA(A) orelse A =:= $_ orelse A =:= $\\ ->
            ephp_parser_class:class_name(Rest0, Parser0, [])
    end,
    Instance = case remove_spaces(Rest1, Parser1) of
        {<<"(",Rest2/binary>>, Parser2} ->
            {Rest3, Parser3, Args} =
                ephp_parser_func:call_args(Rest2, Parser2, []),
            add_line(#instance{name = ObjName,
                               namespace = NS,
                               args = Args}, Parser);
        {Rest3, Parser3} ->
            add_line(#instance{name = ObjName, namespace = NS}, Parser)
    end,
    expression(Rest3, copy_rowcol(Parser3, Parser), add_op(Instance,Parsed));
% CLONE ...
expression(<<C:8,L:8,O:8,N:8,E:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(C,$C,$c) andalso ?OR(L,$L,$l) andalso ?OR(O,$O,$o) andalso
        ?OR(N,$N,$n) andalso ?OR(E,$E,$e) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $() ->
    OpL = <<"clone">>,
    Parser1 = add_pos(Parser, 5),
    Rest1 = <<SP:8, Rest/binary>>,
    expression(Rest1, Parser1, add_op({OpL, precedence(OpL), get_line(Parser)}, Parsed));
% FINAL -enclosed-
expression(<<"}",Rest/binary>>, #parser{level = enclosed} =  Parser, [Exp]) ->
    {Rest, inc_pos(Parser), add_op('end', [Exp])};
% FINAL -unclosed-
expression(<<SP:8,_/binary>> = Rest, #parser{level = unclosed} = Parser, [Exp])
        when ?IS_SPACE(SP) orelse SP =:= $" ->
    {Rest, Parser, add_op('end', [Exp])};
% SPACE -all-
expression(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    expression(Rest, inc_pos(Parser), Parsed);
% NEWLINE -all-
expression(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    expression(Rest, new_line(Parser), Parsed);
% COMMENTS...
expression(<<"//",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_line(Rest, Parser, Parsed),
    expression(Rest0, Parser0, Parsed);
expression(<<"#",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_line(Rest, Parser, Parsed),
    expression(Rest0, Parser0, Parsed);
expression(<<"/*",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_block(Rest, Parser, Parsed),
    expression(Rest0, Parser0, Parsed);
% FUNCTION CALL
expression(<<"(",Rest/binary>>, Parser, [{op, Op}|Parsed]) ->
    {Op1, Op2} = case length(Op) of
        0 ->
            {[], []};
        1 ->
            {[], Op};
        Size ->
            lists:split(Size - 1, Op)
    end,
    case Op2 of
        [#variable{} = V] ->
            Call = #call{name = V, line = V#variable.line},
            {Rest0, Parser0, [Function]} =
                ephp_parser_func:function(Rest, inc_pos(Parser), [Call|Parsed]),
            NewOp = {op, Op1 ++ [Function]},
            expression(Rest0, copy_rowcol(Parser0, Parser), [NewOp|Parsed]);
        _ ->
            exp_parens(Rest, inc_pos(Parser), [{op, Op}|Parsed])
    end;
% PARENS
expression(<<"(", Rest/binary>>, Parser, Parsed) ->
    % FIXME: this is inconsistent, sometimes is expecting to remove ")"
    %        and sometimes is not.
    exp_parens(Rest, Parser, Parsed);
% FINAL -arg-
expression(<<",", _/binary>> = Rest, #parser{level = array} = Parser, Parsed) ->
    {Rest, Parser, add_op('end', Parsed)};
expression(<<",", _/binary>> = Rest, #parser{level = array_curly} = Parser, Parsed) ->
    {Rest, Parser, add_op('end', Parsed)};
expression(<<A:8,_/binary>> = Rest, #parser{level = arg} = Parser, [{op,_}, #if_block{}|_])
        when A =:= $, orelse A =:= $) ->
    throw_error(eparse, Parser, Rest);
expression(<<A:8,_/binary>> = Rest, #parser{level = arg} = Parser, Parsed)
        when A =:= $, orelse A =:= $) ->
    case add_op('end', Parsed) of
        #operation{type = <<"?">>, line = {{_, R}, {_, C}}} ->
            throw_error(eparse, Parser#parser{row = R, col = C}, Rest);
        Op ->
            {Rest, Parser, Op}
    end;
% FINAL -array definition array(...)-
expression(<<A:8,_/binary>> = Rest, #parser{level = array_def, array_type = old} = Parser,
           [{op, _}, #if_block{}|_])
        when A =:= $, orelse A =:= $) ->
    throw_error(eparse, Parser, Rest);
expression(<<A:8,_/binary>> = Rest, #parser{level = array_def, array_type = old} = Parser, [Parsed])
        when A =:= $, orelse A =:= $) ->
    {Rest, Parser, add_op('end', [Parsed])};
expression(<<A:8,_/binary>> = Rest, #parser{level = array_def, array_type = old} = Parser, Parsed)
        when A =:= $, orelse A =:= $) ->
    case Parsed of
        [Arg, Idx] -> {Rest, Parser, [Idx, add_op('end', [Arg])]};
        [] -> {Rest, Parser, undefined}
    end;
% FINAL -array definition [...]-
expression(<<A:8,_/binary>> = Rest, #parser{level = array_def, array_type = php54} = Parser,
           [{op,_}, #if_block{}|_])
        when A =:= $, orelse A =:= $] ->
    throw_error(eparse, Parser, Rest);
expression(<<A:8,_/binary>> = Rest, #parser{level = array_def, array_type = php54} = Parser, [Parsed])
        when A =:= $, orelse A =:= $] ->
    {Rest, Parser, add_op('end', [Parsed])};
expression(<<A:8,_/binary>> = Rest, #parser{level = array_def, array_type = php54} = Parser, Parsed)
        when A =:= $, orelse A =:= $] ->
    case Parsed of
        [Arg, Idx] -> {Rest, Parser, [Idx, add_op('end', [Arg])]};
        [] -> {Rest, Parser, undefined}
    end;
% KEY & VALUE -array_def old and new-
expression(<<"=>",Rest/binary>>, #parser{level = array_def} = Parser, [{op, _} = Op|Parsed]) ->
    expression(Rest, add_pos(Parser, 2), [{op,[]}, add_op('end', [Op])|Parsed]);
% FINAL -all but parens-
expression(<<A:8,_/binary>> = Rest, #parser{level = L} = Parser, [{op, _}, #if_block{}|_])
        when not is_number(L) andalso (A =:= $) orelse A =:= $;) ->
    throw_error(eparse, Parser, Rest);
expression(<<A:8,_/binary>> = Rest, #parser{level = L} = Parser, Parsed)
        when not is_number(L) andalso (A =:= $) orelse A =:= $;) ->
    {Rest, Parser, add_op('end', Parsed)};
% FOREACH DATA
expression(<<A:8,S:8,SP:8,_/binary>> = Rest, #parser{level = foreach_block} = Parser,
           [{op, _}, #if_block{}|_])
        when ?OR(A,$a,$A) andalso ?OR(S,$s,$S) andalso
        not (?IS_ALPHANUM(SP) orelse SP =:= $_ orelse SP =:= $() ->
    throw_error(eparse, Parser, Rest);
expression(<<A:8,S:8,SP:8,_/binary>> = Rest, #parser{level = foreach_block} = Parser, Parsed)
        when ?OR(A,$a,$A) andalso ?OR(S,$s,$S) andalso
        not (?IS_ALPHANUM(SP) orelse SP =:= $_ orelse SP =:= $() ->
    {Rest, Parser, add_op('end', Parsed)};
% FINAL -parens-
expression(<<")",_/binary>> = Rest, #parser{level = L} = Parser, [{op, _}, #if_block{}|_])
        when is_number(L) ->
    throw_error(eparse, Parser, Rest);
expression(<<")",Rest/binary>>, #parser{level = L} = Parser, Parsed) when is_number(L) ->
    {Rest, inc_pos(Parser), add_op('end', Parsed)};
% FINAL -array-
expression(<<"]",_/binary>> = Rest, #parser{level = array} = Parser, [{op, _}, #if_block{}|_]) ->
    throw_error(eparse, Parser, Rest);
expression(<<"]", Rest/binary>>, #parser{level = array} = Parser, Parsed) ->
    {Rest, inc_pos(Parser), add_op('end', Parsed)};
expression(<<"}",_/binary>> = Rest, #parser{level = array_curly} = Parser,
           [{op, _}, #if_block{}|_]) ->
    throw_error(eparse, Parser, Rest);
expression(<<"}", Rest/binary>>, #parser{level = array_curly} = Parser, Parsed) ->
    {Rest, inc_pos(Parser), add_op('end', Parsed)};
% FINAL -all but parens-
expression(<<"?>\n",_/binary>> = Rest, #parser{level = L} = Parser, [{op, _}, #if_block{}|_])
        when not is_number(L) ->
    throw_error(eparse, Parser, Rest);
expression(<<"?>\n",_/binary>> = Rest, #parser{level = L} = Parser, Parsed)
        when not is_number(L) ->
    case add_op('end', Parsed) of
        #operation{type = <<"?">>, line = {{_, R}, {_, C}}} ->
            throw_error(eparse, Parser#parser{row = R, col = C}, Rest);
        Op ->
            {Rest, Parser, Op}
    end;
expression(<<"?>",_/binary>> = Rest, #parser{level = L} = Parser, [{op, _}, #if_block{}|_])
        when not is_number(L) ->
    throw_error(eparse, Parser, Rest);
expression(<<"?>",_/binary>> = Rest, #parser{level = L} = Parser, Parsed)
        when not is_number(L) ->
    {Rest, Parser, add_op('end', Parsed)};
% VARIABLE
expression(<<"$",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, [Var]} = variable(Rest, inc_pos(Parser), []),
    expression(Rest0, copy_rowcol(Parser0, Parser), add_op(Var, Parsed));
% NUMBER
expression(<<A:8, _/binary>> = Rest, Parser, Parsed) when ?IS_NUMBER(A) ->
    {Rest0, Parser0, [Number]} = number(Rest, Parser, []),
    expression(Rest0, copy_rowcol(Parser0, Parser), add_op(Number, Parsed));
expression(<<".", A:8, _/binary>> = Rest, Parser, Parsed) when ?IS_NUMBER(A) ->
    {Rest0, Parser0, [Number]} = number(Rest, Parser, []),
    expression(Rest0, copy_rowcol(Parser0, Parser), add_op(Number, Parsed));
% STRING
expression(<<A:8,_/binary>> = Rest, Parser, Parsed) when
        A =:= $" orelse A =:= $' orelse A =:= $` ->
    {Rest0, Parser0, String} = ephp_parser_string:string(Rest, Parser, []),
    expression(Rest0, Parser0, add_op(String, Parsed));
% HEREDOC & NOWDOC
expression(<<"<<<",_/binary>> = Rest, Parser, Parsed) ->
    {Rest0, Parser0, String} = ephp_parser_string:string(Rest, Parser, []),
    expression(Rest0, Parser0, add_op(String, Parsed));
% INCLUDE
expression(<<I:8,N:8,C:8,L:8,U:8,D:8,E:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(I,$I,$i) andalso ?OR(N,$N,$n) andalso ?OR(C,$C,$c) andalso
        ?OR(L,$L,$l) andalso ?OR(U,$U,$u) andalso ?OR(D,$D,$d) andalso
        ?OR(E,$E,$e) andalso
        (SP =:= $) orelse ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = case remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 7)) of
        {<<"(",R0/binary>>, P0} ->
            {R0, inc_pos(P0)};
        {R0, P0} ->
            {R0, P0}
    end,
    {Rest1, Parser1, Exp} = expression(Rest0, Parser0, []),
    Include = add_line(#call{name = <<"include">>, args = [Exp]}, Parser),
    expression(Rest1, Parser1, add_op(Include, Parsed));
% INCLUDE_ONCE
expression(<<I:8,N:8,C:8,L:8,U:8,D:8,E:8,$_,O:8,N:8,C:8,E:8,SP:8,Rest/binary>>,
           Parser, Parsed) when
        ?OR(I,$I,$i) andalso ?OR(N,$N,$n) andalso ?OR(C,$C,$c) andalso
        ?OR(L,$L,$l) andalso ?OR(U,$U,$u) andalso ?OR(D,$D,$d) andalso
        ?OR(E,$E,$e) andalso ?OR(O,$O,$o) andalso
        (SP =:= $) orelse ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = case remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 12)) of
        {<<"(",R0/binary>>, P0} ->
            {R0, inc_pos(P0)};
        {R0, P0} ->
            {R0, P0}
    end,
    {Rest1, Parser1, Exp} = expression(Rest0, Parser0, []),
    Include = add_line(#call{name = <<"include_once">>, args = [Exp]}, Parser),
    expression(Rest1, Parser1, add_op(Include, Parsed));
% REQUIRE
expression(<<R:8,E:8,Q:8,U:8,I:8,R:8,E:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(R,$R,$r) andalso ?OR(E,$E,$e) andalso ?OR(Q,$Q,$q) andalso
        ?OR(U,$U,$u) andalso ?OR(I,$I,$i) andalso
        (SP =:= $) orelse ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = case remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 7)) of
        {<<"(",R0/binary>>, P0} ->
            {R0, inc_pos(P0)};
        {R0, P0} ->
            {R0, P0}
    end,
    {Rest1, Parser1, Exp} = expression(Rest0, Parser0, []),
    Include = add_line(#call{name = <<"require">>, args = [Exp]}, Parser),
    expression(Rest1, Parser1, add_op(Include, Parsed));
% REQUIRE_ONCE
expression(<<R:8,E:8,Q:8,U:8,I:8,R:8,E:8,$_,O:8,N:8,C:8,E:8,SP:8,Rest/binary>>,
           Parser, Parsed) when
        ?OR(R,$R,$r) andalso ?OR(E,$E,$e) andalso ?OR(Q,$Q,$q) andalso
        ?OR(U,$U,$u) andalso ?OR(I,$I,$i) andalso ?OR(O,$O,$o) andalso
        ?OR(N,$N,$n) andalso ?OR(C,$C,$c) andalso
        (SP =:= $) orelse ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = case remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 12)) of
        {<<"(",R0/binary>>, P0} ->
            {R0, inc_pos(P0)};
        {R0, P0} ->
            {R0, P0}
    end,
    {Rest1, Parser1, Exp} = expression(Rest0, Parser0, []),
    Include = add_line(#call{name = <<"require_once">>, args=[Exp]}, Parser),
    expression(Rest1, Parser1, add_op(Include, Parsed));
% AND
expression(<<A:8,N:8,D:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(A,$a,$A) andalso ?OR(N,$n,$N) andalso ?OR(D,$d,$D)
        andalso (not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_)) ->
    OpL = <<"and">>,
    expression(Rest, add_pos(Parser,3), add_op({OpL,precedence(OpL),Parser}, Parsed));
% XOR
expression(<<X:8,O:8,R:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(X,$x,$X) andalso ?OR(O,$o,$O) andalso ?OR(R,$r,$R)
        andalso (not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_)) ->
    OpL = <<"xor">>,
    expression(Rest, add_pos(Parser, 3), add_op({OpL, precedence(OpL), Parser}, Parsed));
% OPERATOR 3 LETTERS
expression(<<Op:3/binary,Rest/binary>>, Parser, Parsed) when ?IS_OP3(Op) ->
    expression(Rest, add_pos(Parser, 3), add_op({Op, precedence(Op), Parser}, Parsed));
% OR
expression(<<O:8,R:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(O,$o,$O) andalso ?OR(R,$r,$R)
        andalso (not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_)) ->
    OpL = <<"or">>,
    expression(Rest, add_pos(Parser, 2), add_op({OpL, precedence(OpL), Parser}, Parsed));
% OPERATORS 2 LETTERS
expression(<<Op:2/binary,Rest/binary>>, Parser, Parsed) when ?IS_OP2(Op) ->
    expression(Rest, add_pos(Parser, 2), add_op({Op, precedence(Op), Parser}, Parsed));
% OPERATORS 1 LETTER
expression(<<Op:1/binary,Rest/binary>>, Parser, Parsed) when ?IS_OP1(Op) ->
    expression(Rest, inc_pos(Parser), add_op({Op, precedence(Op), Parser}, Parsed));
% CONSTANT / FUNCTION
expression(<<A:8,_/binary>> = Rest, Parser, [{op,[]}|_] = Parsed) when
        ?IS_ALPHA(A) orelse A =:= $_ orelse A =:= $\\ ->
    {Rest0, NewParser, [Constant]} = constant(Rest, Parser, []),
    expression(Rest0, copy_rowcol(NewParser, Parser), add_op(Constant, Parsed));
expression(<<A:8,_/binary>> = Rest, Parser, [{op,Ops}|_]=Parsed) when
        ?IS_ALPHA(A) orelse A =:= $_ orelse A =:= $\\ ->
    {Rest0, Parser0, [Constant]} = constant(Rest, Parser, []),
    case lists:last(Ops) of
        #constant{} ->
            throw_error(eparse, Parser, {Constant#constant.name, <<"T_STRING">>});
        _ ->
            expression(Rest0, copy_rowcol(Parser0, Parser), add_op(Constant, Parsed))
    end;
expression(<<A:8, _/binary>> = Rest, Parser, Parsed) when
        ?IS_ALPHA(A) orelse A =:= $_ orelse A =:= $\\ ->
    case constant(Rest, Parser, []) of
        {Rest0, Parser0, [Constant]} ->
            expression(Rest0, copy_rowcol(Parser0, Parser), add_op(Constant, Parsed));
        {<<>>, _Parser0, []} ->
            {<<>>, Parser, []}
    end;
% FINAL -switch-
expression(<<":", _/binary>> = Rest, #parser{level = switch_label} = Parser, [Exp]) ->
    {Rest, Parser, add_op('end', [Exp])};
% TERNARY OPERATOR
expression(<<"?", Rest/binary>>, Parser, Parsed) ->
    Op = <<"?">>,
    NewParser = inc_pos(Parser),
    QParsed = add_op({Op, precedence(Op), Parser}, Parsed),
    case expression(Rest, NewParser, []) of
        {<<":", Rest0/binary>>, Parser0, Parsed0} ->
            Op0 = <<":">>,
            {Rest1, Parser1, Parsed1} = expression(Rest0, inc_pos(Parser0), []),
            ToAdd = [Parsed0, [{Op0, precedence(Op0), Parser0}], Parsed1],
            XParsed = lists:foldl(fun add_op/2, QParsed, ToAdd),
            {Rest1, Parser1, add_op('end', XParsed)};
        {Rest0, _Parser0, _Parsed0} ->
            throw_error(eparse, Parser, Rest0)
    end;
expression(<<":",Rest/binary>>, Parser, Parsed) ->
    case lists:keyfind(<<"?">>, 1, Parsed) of
        {<<"?">>, _, _} ->
            Op = <<":">>,
            expression(Rest, inc_pos(Parser),
                       add_op({Op, precedence(Op), Parser}, Parsed));
        false ->
            {<<":", Rest/binary>>, Parser, add_op('end', Parsed)}
    end;
% FINAL -unclosed-
expression(Rest, #parser{level = unclosed} = Parser, [Exp]) ->
    {Rest, Parser, add_op('end', [Exp])};
% PARSE ERROR
expression(<<",", _/binary>>, #parser{level = code} = Parser, _Parsed) ->
    throw_error(eparse, Parser, {unexpected, <<",">>});
expression(<<"{", _/binary>>, Parser, _Parsed) ->
    throw_error(eparse, Parser, {unexpected, <<"{">>});
expression(<<>>, Parser, _Parsed) ->
    throw_error(eparse, Parser, <<>>).

exp_parens(Rest, #parser{level = L, col = C} = Parser, Parsed) when not is_number(L) ->
    {Rest0, Parser0, Op} = expression(Rest, Parser#parser{level = 1, col = C + 1}, []),
    expression(Rest0, copy_rowcol(Parser0, Parser), add_op(Op, Parsed));
exp_parens(Rest, #parser{level = L, col = C} = Parser, Parsed) ->
    {Rest0, Parser0, Op} = expression(Rest, Parser#parser{level = L + 1, col = C + 1}, []),
    expression(Rest0, copy_rowcol(Parser0, Parser), add_op(Op, Parsed)).

-type associativity() :: no_assoc | left | right.
-spec precedence(binary()) -> {associativity(), pos_integer()} | false.

%% took from http://php.net/manual/en/language.operators.precedence.php

precedence(<<"clone">>) -> {no_assoc, 1};
precedence(<<"new">>) -> {no_assoc, 1};
precedence(<<"[">>) -> {left, 2}; %% array
precedence(<<"->">>) -> {left, 2}; %% object
precedence(<<"::">>) -> {left, 2}; %% class
precedence(<<"**">>) -> {right, 3}; %% arith
precedence(<<"++">>) -> {right, 4};
precedence(<<"--">>) -> {right, 4};
precedence(<<126>>) -> {right, 4}; %% ~ (types and increment/decrement)
precedence(<<"(int)">>) -> {right, 4};
precedence(<<"(float)">>) -> {right, 4};
precedence(<<"(string)">>) -> {right, 4};
precedence(<<"(array)">>) -> {right, 4};
precedence(<<"(object)">>) -> {right, 4};
precedence(<<"(bool)">>) -> {right, 4};
precedence(<<"(unset)">>) -> {right, 4};
precedence(<<"@">>) -> {right, 1};
precedence(<<"instanceof">>) -> {no_assoc, 5};
precedence(<<"!">>) -> {right, 6}; %% logic
precedence(<<"*">>) -> {left, 7};
precedence(<<"/">>) -> {left, 7};
precedence(<<"%">>) -> {left, 7};
precedence(<<"+">>) -> {left, 8};
precedence(<<"-">>) -> {left, 8};
precedence(<<".">>) -> {left, 8}; %% concat
precedence(<<"<<">>) -> {left, 9}; %% bit by bit
precedence(<<">>">>) -> {left, 9}; %% bit by bit
precedence(<<"<">>) -> {no_assoc, 10};
precedence(<<"<=">>) -> {no_assoc, 10};
precedence(<<">">>) -> {no_assoc, 10};
precedence(<<">=">>) -> {no_assoc, 10};
precedence(<<"==">>) -> {no_assoc, 11};
precedence(<<"!=">>) -> {no_assoc, 11};
precedence(<<"===">>) -> {no_assoc, 11};
precedence(<<"!==">>) -> {no_assoc, 11};
precedence(<<"<>">>) -> {no_assoc, 11};
precedence(<<"<=>">>) -> {no_assoc, 11};
precedence(<<"&">>) -> {left, 12}; %% bit by bit & references
precedence(<<"^">>) -> {left, 13}; %% bit by bit
precedence(<<"|">>) -> {left, 14}; %% bit by bit
precedence(<<"&&">>) -> {left, 15}; %% logic
precedence(<<"||">>) -> {left, 16}; %% logic
precedence(<<"??">>) -> {right, 17}; %% comparison
precedence(<<"?">>) -> {left, 18}; %% ternary
precedence(<<":">>) -> {left, 17}; %% ternary
precedence(<<"=">>) -> {right, 19}; %% assign
precedence(<<"+=">>) -> {right, 19};
precedence(<<"-=">>) -> {right, 19};
precedence(<<"*=">>) -> {right, 19};
precedence(<<"**=">>) -> {right, 19};
precedence(<<"/=">>) -> {right, 19};
precedence(<<".=">>) -> {right, 19};
precedence(<<"%=">>) -> {right, 19};
precedence(<<"&=">>) -> {right, 19};
precedence(<<"|=">>) -> {right, 19};
precedence(<<"^=">>) -> {right, 19};
precedence(<<"<<=">>) -> {right, 19};
precedence(<<">>=">>) -> {right, 19};
precedence(<<"and">>) -> {left, 20};
precedence(<<"xor">>) -> {left, 21};
precedence(<<"or">>) -> {left, 22};
precedence(_) -> false.

operator(<<":">> = Op, Left, Right) ->
    #operation{type = Op, expression_left = Left, expression_right = Right};
operator(<<"?">> = Op, Left, Right) ->
    #operation{type = Op, expression_left = Left, expression_right = Right};
operator(<<"and">>, Left, Right) -> operator('and', Left, Right);
operator(<<"or">>, Left, Right) -> operator('or', Left, Right);
operator(<<"xor">>, Left, Right) -> operator('xor', Left, Right);
operator(<<"&&">>, Left, Right) -> operator('and', Left, Right);
operator(<<"||">>, Left, Right) -> operator('or', Left, Right);
operator(<<"^^">>, Left, Right) -> operator('xor', Left, Right);
operator(<<"<=">>, Left, Right) -> operator(<<"=<">>, Left, Right);
operator(Op, R1, R2) when is_boolean(R1) andalso is_boolean(R2) ->
    case Op of
        'and' -> R1 and R2;
        'or' -> R1 or R2;
        'xor' -> R1 xor R2;
        _ -> #operation{type = Op, expression_left = R1, expression_right = R2}
    end;
operator(<<"/">>, R1, R2) when (is_record(R1, int) orelse is_record(R1, float))
                       andalso (is_record(R2, int) orelse is_record(R2, float)) ->
    N2 = element(2, R2),
    case N2 == 0 of
        true ->
            #operation{type = <<"/">>,
                       expression_left = R1,
                       expression_right = R2};
        false ->
            N1 = element(2, R1),
            if
                N1 rem N2 =:= 0 -> #int{int = N1 div N2};
                true -> #float{float = N1 / N2}
            end
    end;
operator(Op, R1, R2) when (is_record(R1, int) orelse is_record(R1, float))
                  andalso (is_record(R2, int) orelse is_record(R2, float)) ->
    N1 = element(2, R1),
    N2 = element(2, R2),
    Res = case Op of
        <<"+">> -> N1 + N2;
        <<"-">> -> N1 - N2;
        <<"*">> -> N1 * N2;
        <<"**">> -> ephp_data:pow(N1, N2);
        <<"%">> -> N1 rem N2;
        <<">">> -> N1 > N2;
        <<"<">> -> N1 < N2;
        <<"==">> -> N1 == N2;
        <<"===">> -> N1 =:= N2;
        <<"=<">> -> N1 =< N2;
        <<">=">> -> N1 >= N2;
        <<"^">> -> N1 bxor N2;
        <<"&">> -> N1 band N2;
        <<"|">> -> N1 bor N2;
        <<"<<">> -> N1 bsl N2;
        <<">>">> -> N1 bsr N2;
        'xor' -> ephp_data:to_bool(N1) xor ephp_data:to_bool(N2);
        'or' -> ephp_data:to_bool(N1) or ephp_data:to_bool(N2);
        'and' -> ephp_data:to_bool(N1) and ephp_data:to_bool(N2)
    end,
    if
        is_integer(Res) -> #int{int=Res};
        is_float(Res) -> #float{float=Res};
        is_boolean(Res) -> Res
    end;
operator(<<".">>, #text{text = T1}, #text{text = T2}) ->
    #text{text = <<T1/binary,T2/binary>>};
operator(<<".">>, Left, Right) ->
    ProcessedLeft = concat(Left),
    ProcessedRight = concat(Right),
    #concat{texts = ProcessedLeft ++ ProcessedRight};
operator(Op, Left, Right) ->
    #operation{type = Op, expression_left = Left, expression_right = Right}.

concat(#concat{texts = T}) -> T;
concat(T) -> [T].

solve(Expression) ->
    Postfix = shunting_yard(parse_negative(Expression), [], []),
    case gen_op(Postfix, []) of
        [Operation] ->
            Operation;
        [{UnExpected, _, Pos}|_] ->
            throw_error(eparse, Pos, {unexpected, UnExpected})
    end.

gen_op([], Stack) ->
    Stack;
gen_op([{<<"=">>, {_, _}, Parser}|Rest], [B, {operation_not, A, Line}|Stack]) ->
    Assign = add_line(#assign{variable = A, expression = B}, Parser),
    gen_op(Rest, [{operation_not, Assign, Line}|Stack]);
gen_op([{<<"=">>, {_, _}, Parser}|Rest], [B, A|Stack]) ->
    Assign = add_line(#assign{variable = A, expression = B}, Parser),
    gen_op(Rest, [Assign|Stack]);
gen_op([{<<O:1/binary, "=">>, {_, _}, Parser}|Rest], [B, A|Stack])
        when ?IS_OP1_ARITH(O) ->
    Op = add_line(operator(O, A, B), Parser),
    Assign = add_line(#assign{variable = A, expression = Op}, Parser),
    gen_op(Rest, [Assign|Stack]);
gen_op([#variable{} = V, {<<"&">>, _, Parser}|Rest], []) ->
    gen_op(Rest, [add_line(#ref{var = V}, Parser)]);
gen_op([#variable{} = V, {<<"&">>, _, Parser}, {<<"=">>, _, _} = A|Rest], Stack) ->
    gen_op([A|Rest], [add_line(#ref{var=V}, Parser)|Stack]);
gen_op([#call{} = C, {<<"&">>, _, _Parser}|Rest], []) ->
    gen_op(Rest, [C]);
gen_op([#call{} = C, {<<"&">>, _, _Parser}, {<<"=">>, _, _} = A|Rest], Stack) ->
    gen_op([A|Rest], [C|Stack]);
gen_op([{<<"@">>, {right, _}, #parser{}}|Rest], [A|Stack]) ->
    gen_op(Rest, [{silent, A}|Stack]);
gen_op([{<<126>>, {_, _}, #parser{row = R, col = C}}|Rest], [A|Stack]) ->
    gen_op(Rest, [{operation_bnot, A, {{line, R}, {column, C}}}|Stack]);
gen_op([{<<"!">>, {_, _}, #parser{row = R, col = C}}|Rest], [A|Stack]) ->
    gen_op(Rest, [{operation_not, A, {{line, R}, {column, C}}}|Stack]);
gen_op([#constant{name = <<"break">>}, #int{int = I}], []) ->
    [{break, I}];
gen_op([#constant{name = <<"continue">>}, #int{int = I}], []) ->
    [{continue, I}];
gen_op([{<<"->">>, {_, _}, Parser}|Rest], [B, #variable{idx = Idx} = A|Stack]) ->
    Object = case B of
        #int{int = I} -> add_line({object, ephp_data:to_bin(I)}, Parser);
        #float{float = F} -> add_line({object, ephp_data:to_bin(F)}, Parser);
        #constant{name = Name} -> add_line({object, Name}, Parser);
        #variable{} -> add_line({object, B}, Parser);
        #call{} -> add_line({object, B}, Parser);
        #text_to_process{} -> add_line({object, B}, Parser);
        _ -> throw_error(eparse, Parser,
                         {<<"`\"identifier (T_STRING)\"' or "
                            "`\"variable (T_VARIABLE)\"' or "
                            "`'{'' or `'$''">>})
    end,
    gen_op(Rest, [A#variable{idx = Idx ++ [Object]}|Stack]);
gen_op([{<<"++">>, {_, _}, Parser}|Rest], [V|Stack]) ->
    gen_op(Rest, [{post_incr, V, Parser}|Stack]);
gen_op([{<<"--">>, {_, _}, Parser}|Rest], [V|Stack]) ->
    gen_op(Rest, [{post_decr, V, Parser}|Stack]);
gen_op([{<<"::">>, {_, _}, _Parser}|Rest], [#constant{} = A, B|Stack]) ->
    gen_op(Rest, [A#constant{type = class, class = B}|Stack]);
gen_op([{<<"(int)">>, {_, _}, _Parser}|Rest], [#int{} = I|Stack]) ->
    gen_op(Rest, [I|Stack]);
gen_op([{<<"(int)">>, {_, _}, Parser}|Rest], [#float{float = F}|Stack]) ->
    gen_op(Rest, [add_line(#int{int = ephp_data:flooring(F)}, Parser)|Stack]);
gen_op([{<<"(int)">>, {_, _}, Parser}|Rest], [#text{text = T}|Stack]) ->
    gen_op(Rest, [add_line(#int{int = ephp_data:bin_to_number(T)}, Parser)|Stack]);
gen_op([{<<"(int)">>, {_, _}, Parser}|Rest], [A|Stack]) ->
    gen_op(Rest, [add_line(#cast{type = int, content = A}, Parser)|Stack]);
gen_op([{<<"(float)">>, {_, _}, Parser}|Rest], [#int{int = I}|Stack]) ->
    gen_op(Rest, [add_line(#float{float = erlang:float(I)}, Parser)|Stack]);
gen_op([{<<"(float)">>, {_, _}, _Parser}|Rest], [#float{} = F|Stack]) ->
    gen_op(Rest, [F|Stack]);
gen_op([{<<"(float)">>, {_, _}, Parser}|Rest], [#text{text = T}|Stack]) ->
    Float = erlang:float(ephp_data:bin_to_number(T)),
    gen_op(Rest, [add_line(#float{float = Float}, Parser)|Stack]);
gen_op([{<<"(float)">>, {_, _}, Parser}|Rest], [A|Stack]) ->
    gen_op(Rest, [add_line(#cast{type = float, content = A}, Parser)|Stack]);
gen_op([{<<"(string)">>, {_, _}, Parser}|Rest], [#int{int = I}|Stack]) ->
    gen_op(Rest, [add_line(#text{text = ephp_data:to_bin(I)}, Parser)|Stack]);
gen_op([{<<"(string)">>, {_, _}, _Parser}|Rest], [#text{} = T|Stack]) ->
    gen_op(Rest, [T|Stack]);
gen_op([{<<"(string)">>, {_, _}, Parser}|Rest], [#float{float = F}|Stack]) ->
    gen_op(Rest, [add_line(#text{text = ephp_data:to_bin(F)}, Parser)|Stack]);
gen_op([{<<"(string)">>, {_, _}, Parser}|Rest], [A|Stack]) ->
    gen_op(Rest, [add_line(#cast{type = string, content = A}, Parser)|Stack]);
gen_op([{<<"(bool)">>, {_, _}, _Parser}|Rest], [#int{int = I}|Stack]) ->
    gen_op(Rest, [ephp_data:to_bool(I)|Stack]);
gen_op([{<<"(bool)">>, {_, _}, _Parser}|Rest], [#text{text = T}|Stack]) ->
    gen_op(Rest, [ephp_data:to_bool(T)|Stack]);
gen_op([{<<"(bool)">>, {_, _}, _Parser}|Rest], [#float{float = F}|Stack]) ->
    gen_op(Rest, [ephp_data:to_bool(F)|Stack]);
gen_op([{<<"(bool)">>, {_, _}, Parser}|Rest], [A|Stack]) ->
    gen_op(Rest, [add_line(#cast{type = bool, content = A}, Parser)|Stack]);
gen_op([{<<"(array)">>, {_, _}, Parser}|Rest], [#int{line = DParser} = D|Stack]) ->
    gen_op(Rest, [add_to_array(#array{line = DParser}, Parser, D)|Stack]);
gen_op([{<<"(array)">>, {_, _}, Parser}|Rest], [#text{line = DParser} = D|Stack]) ->
    gen_op(Rest, [add_to_array(#array{line = DParser}, Parser, D)|Stack]);
gen_op([{<<"(array)">>, {_, _}, Parser}|Rest], [#float{line = DParser} = D|Stack]) ->
    gen_op(Rest, [add_to_array(#array{line = DParser}, Parser, D)|Stack]);
gen_op([{<<"(array)">>, {_, _}, Parser}|Rest], [A|Stack]) ->
    gen_op(Rest, [add_line(#cast{type = array, content = A}, Parser)|Stack]);
gen_op([{<<"(object)">>, {_, _}, Parser}|Rest], [A|Stack]) ->
    gen_op(Rest, [add_line(#cast{type = object, content = A}, Parser)|Stack]);
gen_op([{<<"(unset)">>, {_, _}, _Parser}|Rest], [_|Stack]) ->
    gen_op(Rest, [undefined|Stack]);
gen_op([{<<"clone">>, {_, _}, Parser}|Rest], [A|Stack]) ->
    gen_op(Rest, [add_line(#clone{var = A}, Parser)|Stack]);
% TODO add the rest of casting operators
gen_op([{<<"??">>, {_, _}, Parser}|Rest], [B, A|Stack]) ->
    Isset = add_line(#call{name = <<"isset">>, args = [A]}, Parser),
    IfBlock = add_line(#if_block{conditions = Isset,
                                 true_block = A,
                                 false_block = B}, Parser),
    gen_op(Rest, [IfBlock|Stack]);
gen_op([{<<"?">>, {_, _}, Parser}|Rest],
       [#operation{type = <<":">>} = OpElse, Cond|Stack]) ->
    #operation{expression_left = TrueBlock,
               expression_right = FalseBlock} = OpElse,
    IfBlock = add_line(#if_block{conditions = Cond,
                                 true_block = TrueBlock,
                                 false_block = FalseBlock}, Parser),
    gen_op(Rest, [IfBlock|Stack]);
gen_op([{<<"?">>, {_, _}, Parser}|_Rest], _Stack) ->
    throw_error(eparse, Parser, <<>>);
gen_op([{Op, {_, _}, Parser}|Rest], [B, A|Stack]) ->
    gen_op(Rest, [add_line(operator(Op, A, B), Parser)|Stack]);
gen_op([A|Rest], Stack) ->
    gen_op(Rest, [A|Stack]).

add_to_array(#array{elements = E} = Array, Parser, Element) ->
    Array#array{elements = E ++ [
        add_line(#array_element{element = Element}, Parser)
    ]}.

parse_negative(Elements) ->
    parse_negative(lists:reverse(Elements), []).

parse_negative([#int{} = I, {<<"-">>, {_, _}, _}, {_, {_, _}, _} = Op|Rest], Stack) ->
    parse_negative([I#int{int = -I#int.int}, Op|Rest], Stack);
parse_negative([#float{} = F, {<<"-">>, {_, _}, _}, {_, {_, _}, _} = Op|Rest], Stack) ->
    parse_negative([F#float{float = -F#float.float}, Op|Rest], Stack);
parse_negative([A, {<<"-">>, {_, _}, _}, {_, {_, _}, _} = Op|Rest], Stack) ->
    parse_negative([{operation_minus, A, undefined}, Op|Rest], Stack);
parse_negative([#int{} = I, {<<"-">>, {_, _}, _}], Stack) ->
    [I#int{int = -I#int.int}|Stack];
parse_negative([#float{} = F, {<<"-">>, {_, _}, _}], Stack) ->
    [F#float{float = -F#float.float}|Stack];
parse_negative([A, {<<"-">>, {_, _}, _}], Stack) ->
    [{operation_minus, A, undefined}|Stack];
parse_negative([A|Rest], Stack) ->
    parse_negative(Rest, [A|Stack]);
parse_negative([], Stack) ->
    Stack.

shunting_yard([], [], Postfix) ->
    Postfix;
shunting_yard([], OpS, Postfix) ->
    Postfix ++ OpS;
shunting_yard([{_, {_, _}, _} = Op|Rest], [], Postfix) ->
    shunting_yard(Rest, [Op], Postfix);
shunting_yard([{_, {left, P1}, _} = Op|Rest], [{_, {_, P2}, _} = Op1|OpS], Postfix)
        when P1 > P2 ->
    shunting_yard(Rest, [Op|OpS], Postfix ++ [Op1]);
shunting_yard([{_, {_, P1}, _} = Op|Rest], [{_, {_, P2}, _} = Op1|OpS], Postfix)
        when P1 >= P2 ->
    shunting_yard(Rest, [Op|OpS], Postfix ++ [Op1]);
shunting_yard([{_, {left, P1}, _} = Op|Rest], [{_, {_, P2}, _}|_] = OpS, Postfix)
        when P1 =< P2 ->
    shunting_yard(Rest, [Op|OpS], Postfix);
shunting_yard([{_, {_, P1}, _} = Op|Rest], [{_, {_, P2}, _}|_] = OpS, Postfix)
        when P1 < P2 ->
    shunting_yard(Rest, [Op|OpS], Postfix);
shunting_yard([A|Rest], OpS, Postfix) ->
    shunting_yard(Rest, OpS, Postfix ++ [A]).

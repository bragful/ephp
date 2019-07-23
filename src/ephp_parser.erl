-module(ephp_parser).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([parse/1, file/1]).

-export([
    add_pos/2, new_line/1, copy_rowcol/2, add_line/2, remove_spaces/2,
    throw_error/3, code_block/3, code/3, new_line/2, inc_pos/1,
    get_ns/2,

    array_def_level/1, code_statement_level/1, arg_level/1,
    code_block_level/1, enclosed_level/1, unclosed_level/1,
    get_line/1,

    variable/3, comment_line/3, comment_block/3, constant/3
]).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-import(ephp_parser_expr, [
    expression/3, add_op/2, precedence/1
]).

file(File) ->
    {ok, Content} = file:read_file(File),
    parse(Content).

parse(Document) when is_list(Document) ->
    parse(list_to_binary(Document));
parse(Document) ->
    {_Rest, _Parser, Parsed} = document(Document, #parser{}, []),
    lists:reverse(Parsed).

document(<<>>, Parser, Parsed) ->
    {<<>>, Parser, Parsed};
document(<<"<?php",Rest/binary>>, #parser{level = literal} = Parser, Parsed) ->
    {Rest, add_pos(Parser, 5), Parsed};
document(<<"<?php",Rest/binary>>, Parser, Parsed) ->
    Parser0 = normal_level(add_pos(Parser, 5)),
    {Rest1, Parser1, NParsed} = code_ns(Rest, Parser0, []),
    case NParsed of
        [] ->
            document(Rest1, Parser1, Parsed);
        _ ->
            Eval = add_line(#eval{statements = lists:reverse(NParsed)}, Parser),
            document(Rest1, Parser1, [Eval|Parsed])
    end;
document(<<"<?=", Rest/binary>>, Parser, Parsed) ->
    NewParser = code_value_level(add_pos(Parser, 3)),
    {Rest0, Parser0, Text} = code_ns(Rest, NewParser, []),
    document(Rest0, copy_rowcol(Parser0, Parser), [get_print(Text, NewParser)|Parsed]);
document(<<"<?", Rest/binary>>, #parser{level = literal} = Parser, Parsed) ->
    %% TODO: if short is not permitted, use as text
    {Rest, add_pos(Parser, 2), Parsed};
document(<<"<?", Rest/binary>>, Parser, Parsed) ->
    %% TODO: if short is not permitted, use as text
    {Rest0, Parser0, NParsed} = code_ns(Rest, normal_level(add_pos(Parser, 2)), []),
    case NParsed of
        [] ->
            document(Rest0, Parser0, Parsed);
        _ ->
            Eval = add_line(#eval{statements = lists:reverse(NParsed)}, Parser),
            document(Rest0, Parser0, [Eval|Parsed])
    end;
document(<<"\n",Rest/binary>>, Parser, Parsed) ->
    Parser1 = new_line(maybe_set_namespace(Parser)),
    document(Rest, Parser1, add_to_text(<<"\n">>, Parser1, Parsed));
document(<<L:1/binary,Rest/binary>>, Parser, Parsed) ->
    Parser1 = inc_pos(maybe_set_namespace(Parser)),
    document(Rest, Parser1, add_to_text(L, Parser1, Parsed)).


maybe_set_namespace(#parser{namespace_can_be = true} = Parser) ->
    Parser#parser{namespace_can_be = false};
maybe_set_namespace(#parser{namespace_can_be = only_block} = Parser) ->
    throw_error(enamespaceblock, Parser, ?E_ERROR, undefined);
maybe_set_namespace(Parser) -> Parser.


code_ns(<<N:8,A:8,M:8,E:8,S:8,P:8,A:8,C:8,E:8,SP:8,Rest/binary>>,
        #parser{namespace = [], namespace_can_be = NScanBe} = Parser,
        Parsed) when
            ?OR(N,$N,$n) andalso ?OR(A,$A,$a) andalso ?OR(M,$M,$m) andalso
            ?OR(E,$E,$e) andalso ?OR(S,$S,$s) andalso ?OR(P,$P,$p) andalso
            ?OR(C,$C,$c) andalso
            (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) andalso
            NScanBe =/= false ->
    {Rest0, Parser0, NameSpace} = namespace(<<SP:8, Rest/binary>>,
                                            add_pos(Parser, 9), []),
    case remove_spaces(Rest0, Parser0#parser{namespace = NameSpace,
                                             namespace_can_be = false}) of
        {<<";", _/binary>> = Rest1, Parser1} when NScanBe =:= true
                                           orelse NScanBe =:= only_statement ->
            code(Rest1,
                 Parser1#parser{namespace_can_be = only_statement},
                 Parsed);
        {<<";", _/binary>>, Parser1} when NScanBe =:= only_block ->
            throw_error(enamespaceblock, Parser1, ?E_ERROR, undefined);
        {<<"{", _/binary>>, Parser1} when NScanBe =:= only_statement ->
            throw_error(enamespacemix, Parser1, ?E_ERROR, undefined);
        {<<"{", _/binary>> = Rest1, Parser1} when NScanBe =:= only_block
                                           orelse NScanBe =:= true ->
            {Rest2, Parser2, CodeBlock} = code_block(Rest1, Parser1, []),
            Parser2_1 = Parser2#parser{namespace = [],
                                       namespace_can_be = only_block},
            code_ns(Rest2, Parser2_1, lists:reverse(CodeBlock) ++ Parsed)
    end;
code_ns(<<"//", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_line(Rest, add_pos(Parser, 2), Parsed),
    code_ns(Rest0, Parser0, Parsed);
code_ns(<<"#", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_line(Rest, inc_pos(Parser), Parsed),
    code_ns(Rest0, Parser0, Parsed);
code_ns(<<"/*", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_block(Rest, add_pos(Parser, 2), Parsed),
    code_ns(Rest0, Parser0, Parsed);
code_ns(<<Space:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(Space) ->
    code_ns(Rest, inc_pos(Parser), Parsed);
code_ns(<<NewLine:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(NewLine) ->
    code_ns(Rest, new_line(Parser), Parsed);
code_ns(Rest, Parser, Parsed) ->
    code(Rest, Parser, Parsed).


code(<<"{", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, Parsed0} = code(Rest, code_block_level(add_pos(Parser, 2)), []),
    code(Rest0, Parser0, Parsed0 ++ Parsed);
code(<<B:8,R:8,E:8,A:8,K:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(B,$B,$b) andalso ?OR(R,$R,$r) andalso ?OR(E,$E,$e) andalso
        ?OR(A,$A,$a) andalso ?OR(K,$K,$k) andalso
        (not (?IS_SPACE(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_)) ->
    code(<<SP:8,Rest/binary>>, add_pos(Parser, 5), [break|Parsed]);
code(<<C:8,O:8,N:8,T:8,I:8,N:8,U:8,E:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(C,$C,$c) andalso ?OR(O,$O,$o) andalso ?OR(N,$N,$n) andalso
        ?OR(T,$T,$t) andalso ?OR(I,$I,$i) andalso ?OR(U,$U,$u) andalso
        ?OR(E,$E,$e) andalso (not (?IS_SPACE(SP) orelse ?IS_NUMBER(SP))) ->
    code(<<SP:8,Rest/binary>>, add_pos(Parser, 8), [continue|Parsed]);
code(<<R:8,E:8,T:8,U:8,R:8,N:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(R,$R,$r) andalso ?OR(E,$E,$e) andalso ?OR(T,$T,$t) andalso
        ?OR(U,$U,$u) andalso ?OR(N,$N,$n) andalso
        (not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP))) ->
    {Rest0, Parser0, Return} = expression(<<SP:8,Rest/binary>>, add_pos(Parser, 6), []),
    case Return of
        [] -> code(Rest0, Parser0, [add_line(#return{}, Parser)|Parsed]);
        _ -> code(Rest0, Parser0, [add_line(#return{value = Return}, Parser)|Parsed])
    end;
code(<<T:8,H:8,R:8,O:8,W:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(T,$T,$t) andalso ?OR(H,$H,$h) andalso ?OR(R,$R,$r) andalso
        ?OR(O,$O,$o) andalso ?OR(W,$W,$w) andalso
        (not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_)) ->
    {Rest0, Parser0, Throw} = expression(<<SP:8, Rest/binary>>, add_pos(Parser, 5), []),
    case Throw of
        [] -> throw_error(eparse, Parser0, Rest);
        _ -> code(Rest0, Parser0, [add_line(#throw{value = Throw}, Parser)|Parsed])
    end;
code(<<T:8,R:8,Y:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(T,$T,$t) andalso ?OR(R,$R,$r) andalso ?OR(Y,$Y,$y) andalso
        (not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_)) ->
    {Rest0, Parser0, Code} = code_block(<<SP:8, Rest/binary>>, add_pos(Parser, 3), []),
    code(Rest0, copy_rowcol(Parser0, Parser),
         [add_line(#try_catch{code_block = Code}, Parser)|Parsed]);
%% TODO catch must be part of try, couldn't be separate!
code(<<C:8,A:8,T:8,C:8,H:8,SP:8,Rest/binary>>, Parser, [#try_catch{} = Try|Parsed])
        when ?OR(C,$C,$c) andalso ?OR(A,$A,$a) andalso ?OR(T,$T,$t)
        andalso ?OR(H,$H,$h) andalso
        (not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_)) ->
    {<<"(", Rest0/binary>>, Parser0} =
        remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 5)),
    {Rest1, Parser1, [Arg]} = ephp_parser_func:funct_args(Rest0, inc_pos(Parser0), []),
    {Rest2, Parser2, Code} = code_block(Rest1, Parser1, []),
    #try_catch{catches = Catches} = Try,
    NewCatch = add_line(#catch_block{exception = Arg, code_block = Code}, Parser),
    NewTry = Try#try_catch{catches = Catches ++ [NewCatch]},
    code(Rest2, copy_rowcol(Parser2, Parser), [NewTry|Parsed]);
%% TODO finally must be part of try, couldn't be separate!
code(<<F:8,I:8,N:8,A:8,L:8,L:8,Y:8,SP:8,Rest/binary>>, Parser,
     [#try_catch{} = Try|Parsed]) when
        ?OR(F,$F,$f) andalso ?OR(I,$I,$i) andalso ?OR(N,$N,$n) andalso
        ?OR(A,$A,$a) andalso ?OR(L,$L,$l) andalso ?OR(Y,$Y,$y) andalso
        (not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_)) ->
    {Rest0, Parser0, Code} = code_block(<<SP:8, Rest/binary>>, add_pos(Parser, 7), []),
    code(Rest0, copy_rowcol(Parser0, Parser), [Try#try_catch{finally = Code}|Parsed]);
code(<<"@", _/binary>> = Rest, Parser, Parsed) ->
    {Rest0, Parser0, Exp} = expression(Rest, code_statement_level(Parser), []),
    code(Rest0, copy_rowcol(Parser0, Parser), [Exp|Parsed]);
code(<<G:8,L:8,O:8,B:8,A:8,L:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(G,$G,$g) andalso ?OR(L,$L,$l) andalso ?OR(O,$O,$o) andalso
        ?OR(B,$B,$b) andalso ?OR(A,$A,$a) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = remove_spaces(Rest, add_pos(Parser, 7)),
    {Rest1, Parser1, [Global]} = st_global(Rest0, Parser0, []),
    code(Rest1, copy_rowcol(Parser1, Parser), [Global|Parsed]);
code(<<"}",Rest/binary>>, #parser{level = code_block} = Parser, Parsed) ->
    {Rest, inc_pos(Parser), lists:reverse(Parsed)};
code(<<"}",Rest/binary>>, #parser{level = switch_block} = Parser, Parsed) ->
    {Rest, inc_pos(Parser), lists:reverse(switch_case_block(Parsed))};
code(<<E:8,N:8,D:8,I:8,F:8,SP:8,Rest/binary>>, #parser{level = if_old_block} = Parser, Parsed)
    when
        ?OR(E,$E,$e) andalso ?OR(N,$N,$n) andalso ?OR(D,$D,$d) andalso
        ?OR(I,$I,$i) andalso ?OR(F,$F,$f) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $;) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 5)),
    {Rest0, Parser0, lists:reverse(Parsed)};
code(<<E:8,N:8,D:8,S:8,W:8,I:8,T:8,C:8,H:8,SP:8,Rest/binary>>,
     #parser{level = switch_old_block} = Parser, Parsed) when
        ?OR(E,$E,$e) andalso ?OR(N,$N,$n) andalso ?OR(D,$D,$d) andalso
        ?OR(S,$S,$s) andalso ?OR(W,$W,$w) andalso ?OR(I,$I,$i) andalso
        ?OR(T,$T,$t) andalso ?OR(C,$C,$c) andalso ?OR(H,$H,$h) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $;) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 9)),
    {Rest0, Parser0, lists:reverse(switch_case_block(Parsed))};
code(<<E:8,N:8,D:8,F:8,O:8,R:8,E:8,A:8,C:8,H:8,SP:8,Rest/binary>>,
     #parser{level = foreach_old_block} = Parser, Parsed) when
        ?OR(E,$E,$e) andalso ?OR(N,$N,$n) andalso ?OR(D,$D,$d) andalso
        ?OR(F,$F,$f) andalso ?OR(O,$O,$o) andalso ?OR(R,$R,$r) andalso
        ?OR(A,$A,$a) andalso ?OR(C,$C,$c) andalso ?OR(H,$H,$h) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $;) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 10)),
    {Rest0, Parser0, lists:reverse(Parsed)};
code(<<E:8,N:8,D:8,F:8,O:8,R:8,SP:8,Rest/binary>>,
     #parser{level = for_old_block} = Parser, Parsed) when
        ?OR(E,$E,$e) andalso ?OR(N,$N,$n) andalso ?OR(D,$D,$d) andalso
        ?OR(F,$F,$f) andalso ?OR(O,$O,$o) andalso ?OR(R,$R,$r) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $;) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 6)),
    {Rest0, Parser0, lists:reverse(Parsed)};
code(<<E:8,N:8,D:8,W:8,H:8,I:8,L:8,E:8,SP:8,Rest/binary>>,
     #parser{level = while_old_block} = Parser, Parsed) when
        ?OR(E,$E,$e) andalso ?OR(N,$N,$n) andalso ?OR(D,$D,$d) andalso
        ?OR(W,$W,$w) andalso ?OR(H,$H,$h) andalso ?OR(I,$I,$i) andalso
        ?OR(L,$L,$l) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $;) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 8)),
    {Rest0, Parser0, lists:reverse(Parsed)};
code(<<A:8,_/binary>> = Rest, #parser{level = code_statement} = Parser, Parsed)
        when A =:= $; orelse A =:= $} ->
    {Rest, Parser, Parsed};
code(<<T:8,R:8,U:8,E:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(T,$t,$T) andalso ?OR(R,$r,$R) andalso ?OR(U,$u,$U)
        andalso ?OR(E,$e,$E) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0, Exp} = expression(Rest, Parser, add_op(true, [])),
    code(Rest0, copy_rowcol(Parser0, Parser), [Exp|Parsed]);
code(<<F:8,A:8,L:8,S:8,E:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(F,$f,$F) andalso ?OR(A,$a,$A) andalso ?OR(L,$l,$L)
        andalso ?OR(S,$s,$S) andalso ?OR(E,$e,$E)
        andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0, Exp} = expression(Rest, Parser, add_op(false, [])),
    code(Rest0, copy_rowcol(Parser0, Parser), [Exp|Parsed]);
code(<<I:8,F:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(I,$i,$I) andalso ?OR(F,$f,$F)
        andalso (?IS_SPACE(SP) orelse SP =:= $() ->
    {Rest0, Parser0} = remove_spaces(<<SP:8,Rest/binary>>, Parser),
    {Rest1, Parser1, [If]} = st_if(Rest0, Parser0, []),
    code(Rest1, copy_rowcol(Parser1, Parser), [If|Parsed]);
code(<<W:8,H:8,I:8,L:8,E:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(W,$w,$W) andalso ?OR(H,$h,$H) andalso ?OR(I,$i,$I)
        andalso ?OR(L,$l,$L) andalso ?OR(E,$e,$E)
        andalso (?IS_SPACE(SP) orelse SP =:= $() ->
    {Rest0, Parser0} = remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 5)),
    {Rest1, Parser1, NewParsed} = st_while(Rest0, Parser0, Parsed),
    code(Rest1, copy_rowcol(Parser1, Parser), NewParsed);
code(<<D:8,O:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(D,$d,$D) andalso ?OR(O,$o,$O) andalso
        (?IS_SPACE(SP) orelse ?OR(SP,${,$:) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0, [DoWhile]} = st_do_while(Rest, add_pos(Parser, 3), []),
    code(Rest0, copy_rowcol(Parser0, Parser), [DoWhile|Parsed]);
code(<<F:8,O:8,R:8,E:8,A:8,C:8,H:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(F,$f,$F) andalso ?OR(O,$o,$O) andalso ?OR(R,$r,$R)
        andalso ?OR(E,$e,$E) andalso ?OR(A,$a,$A) andalso ?OR(C,$c,$C)
        andalso ?OR(H,$h,$H)
        andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $() ->
    {Rest0, Parser0} = remove_spaces(<<SP:8, Rest/binary>>, Parser),
    {Rest1, Parser1, NewParsed} = st_foreach(Rest0, Parser0, Parsed),
    code(Rest1, copy_rowcol(Parser1, Parser), NewParsed);
code(<<F:8,O:8,R:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(F,$f,$F) andalso ?OR(O,$o,$O) andalso ?OR(R,$r,$R)
        andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $() ->
    {Rest0, Parser0} = remove_spaces(<<SP:8,Rest/binary>>, Parser),
    {Rest1, Parser1, NewParsed} = st_for(Rest0, Parser0, Parsed),
    code(Rest1, copy_rowcol(Parser1, Parser), NewParsed);
code(<<E:8,L:8,S:8,E:8,SP:8,_/binary>> = Rest, #parser{level = if_old_block} = Parser, Parsed)
        when ?OR(E,$e,$E) andalso ?OR(L,$l,$L) andalso ?OR(S,$s,$S)
        andalso (SP =:= $: orelse ?IS_SPACE(SP) orelse ?OR(SP,$i,$I)) ->
    {Rest, Parser, Parsed};
code(<<S:8,W:8,I:8,T:8,C:8,H:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(S,$S,$s) andalso ?OR(W,$W,$w) andalso ?OR(I,$I,$i) andalso
        ?OR(T,$T,$t) andalso ?OR(C,$C,$c) andalso ?OR(H,$H,$h) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $() ->
    {<<"(",_/binary>> = Rest0, Parser0} = remove_spaces(<<SP:8,Rest/binary>>, Parser),
    {Rest1, Parser1, NewParsed} = st_switch(Rest0, add_pos(Parser0, 6), Parsed),
    code(Rest1, copy_rowcol(Parser1, Parser), NewParsed);
code(<<C:8,A:8,S:8,E:8,SP:8,Rest/binary>>, #parser{level = Level} = Parser, Parsed) when
        (Level =:= switch_block orelse Level =:= switch_old_block) andalso
        ?OR(C,$C,$c) andalso ?OR(A,$A,$a) andalso ?OR(S,$S,$s) andalso
        ?OR(E,$E,$e) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 4)),
    NewParser = switch_label_level(Parser0),
    case expression(Rest0, NewParser, []) of
        {<<":", Rest1/binary>>, Parser1, Exp} -> ok;
        {<<";", Rest1/binary>>, Parser1, Exp} -> ok
    end,
    NewParsed = [add_line(#switch_case{label = Exp, code_block = []}, Parser)|
                 switch_case_block(Parsed)],
    code(Rest1, copy_rowcol(inc_pos(Parser1), Parser), NewParsed);
code(<<C:8,A:8,S:8,E:8,SP:8,Rest/binary>>, #parser{level = code_statement} = Parser, Parsed) when
        ?OR(C,$C,$c) andalso ?OR(A,$A,$a) andalso ?OR(S,$S,$s) andalso
        ?OR(E,$E,$e) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {<<"case",SP:8,Rest/binary>>, Parser, Parsed};
code(<<D:8,E:8,F:8,A:8,U:8,L:8,T:8,SP:8,Rest/binary>>,
     #parser{level = Level} = Parser, Parsed) when
        (Level =:= switch_block orelse Level =:= switch_old_block) andalso
        ?OR(D,$D,$d) andalso ?OR(E,$E,$e) andalso ?OR(F,$F,$f) andalso
        ?OR(A,$A,$a) andalso ?OR(U,$U,$u) andalso ?OR(L,$L,$l) andalso
        ?OR(T,$T,$t) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $:) ->
    case remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 4)) of
        {<<":", Rest0/binary>>, Parser0} -> ok;
        {<<";", Rest0/binary>>, Parser0} -> ok
    end,
    NewParsed = [add_line(#switch_case{label = default,
                                       code_block = []},
                          Parser)|switch_case_block(Parsed)],
    code(Rest0, copy_rowcol(inc_pos(Parser0), Parser), NewParsed);
code(<<D:8,E:8,F:8,A:8,U:8,L:8,T:8,SP:8,Rest/binary>>,
     #parser{level = code_statement} = Parser, Parsed) when
        ?OR(D,$D,$d) andalso ?OR(E,$E,$e) andalso ?OR(F,$F,$f) andalso
        ?OR(A,$A,$a) andalso ?OR(U,$U,$u) andalso ?OR(L,$L,$l) andalso
        ?OR(T,$T,$t) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $:) ->
    {<<"default",SP:8,Rest/binary>>, Parser, Parsed};
code(<<A:8,B:8,S:8,T:8,R:8,A:8,C:8,T:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(A,$A,$a) andalso ?OR(B,$B,$b) andalso ?OR(S,$S,$s) andalso
        ?OR(T,$T,$t) andalso ?OR(R,$R,$r) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0, Parsed0} = code(Rest, add_pos(Parser, 9), []),
    Class = lists:last(Parsed0),
    {Rest0, Parser0, Parsed0 ++ [Class#class{type = abstract}] ++ Parsed};
code(<<F:8,I:8,N:8,A:8,L:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(F,$F,$f) andalso ?OR(I,$I,$i) andalso ?OR(N,$N,$n) andalso
        ?OR(A,$A,$a) andalso ?OR(L,$L,$l) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    Class = add_line(#class{final = true}, Parser),
    code(<<SP:8, Rest/binary>>, add_pos(Parser, 5), [Class|Parsed]);
code(<<C:8,L:8,A:8,S:8,S:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(C,$C,$c) andalso ?OR(L,$L,$l) andalso ?OR(A,$A,$a) andalso
        ?OR(S,$S,$s) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0, Class0} = case Parsed of
        [#class{name = undefined} = Class|_] ->
            ephp_parser_class:st_class(<<SP:8, Rest/binary>>, add_pos(Parser, 5),
                                       Class);
        _ ->
            Class = add_line(#class{}, Parser),
            ephp_parser_class:st_class(<<SP:8, Rest/binary>>, add_pos(Parser, 5),
                                       Class)
    end,
    code(Rest0, copy_rowcol(Parser0, Parser), [Class0|Parsed]);
code(<<U:8,S:8,E:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(U,$U,$u) andalso ?OR(S,$S,$s) andalso ?OR(E,$E,$e) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    case remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 3)) of
        {<<C:8,O:8,N:8,S:8,T:8,SP:8,Rest0/binary>>, Parser0} when
                ?OR(C,$C,$c) andalso ?OR(O,$O,$o) andalso ?OR(N,$N,$n) andalso
                ?OR(S,$S,$s) andalso ?OR(T,$T,$t) andalso
                (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
            use_const(<<SP:8,Rest0/binary>>, add_pos(Parser0, 5), Parsed);
        {<<F:8,U:8,N:8,C:8,T:8,I:8,O:8,N:8,SP:8,Rest0/binary>>, Parser0} when
                ?OR(F,$F,$f) andalso ?OR(U,$U,$u) andalso ?OR(N,$N,$n) andalso
                ?OR(C,$C,$c) andalso ?OR(T,$T,$t) andalso ?OR(I,$I,$i) andalso
                ?OR(O,$O,$o) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
            use_function(<<SP:8,Rest0/binary>>, add_pos(Parser0, 8), Parsed);
        {Rest0, Parser0} ->
            use_list(Rest0, Parser0#parser{use_ns = []}, Parsed)
    end;
code(<<N:8,A:8,M:8,E:8,S:8,P:8,A:8,C:8,E:8,SP:8,_/binary>> = Rest,
     #parser{namespace_can_be = only_statement} = Parser, Parsed) when
        ?OR(N,$N,$n) andalso ?OR(A,$A,$a) andalso ?OR(M,$M,$m) andalso
        ?OR(E,$E,$e) andalso ?OR(S,$S,$s) andalso ?OR(P,$P,$p) andalso
        ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    code_ns(Rest, Parser#parser{namespace = []}, Parsed);
code(<<N:8,A:8,M:8,E:8,S:8,P:8,A:8,C:8,E:8,SP:8,_/binary>>, Parser, _Parsed) when
        ?OR(N,$N,$n) andalso ?OR(A,$A,$a) andalso ?OR(M,$M,$m) andalso
        ?OR(E,$E,$e) andalso ?OR(S,$S,$s) andalso ?OR(P,$P,$p) andalso
        ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    throw_error(enamespace, Parser, ?E_ERROR, undefined);
code(<<I:8,N:8,T:8,E:8,R:8,F:8,A:8,C:8,E:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(I,$I,$i) andalso ?OR(N,$N,$n) andalso ?OR(T,$T,$t) andalso
        ?OR(E,$E,$e) andalso ?OR(R,$R,$r) andalso ?OR(F,$F,$f) andalso
        ?OR(A,$A,$a) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    Interface = add_line(#class{type = interface}, Parser),
    {Rest0, Parser0, Interface0} =
        ephp_parser_class:st_interface(<<SP:8, Rest/binary>>, add_pos(Parser, 9),
                                       Interface),
    code(Rest0, copy_rowcol(Parser0, Parser), [Interface0|Parsed]);
code(<<E:8,C:8,H:8,O:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(E,$e,$E) andalso ?OR(C,$c,$C) andalso ?OR(H,$h,$H) andalso
        ?OR(O,$o,$O) andalso
        (SP =:= $( orelse ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    Call = add_line(#call{name = <<"print">>}, Parser),
    {Rest0, Parser0, [Call0]} =
        ephp_parser_func:echo(<<SP:8,Rest/binary>>, add_pos(Parser, 4), [Call]),
    % FIXME if we detect an OR or AND expression, we put around print
    case Call0#call.args of
        [#operation{type = Type} = A1] when Type =:= 'or' orelse Type =:= 'and' ->
            Print = A1#operation{
                expression_left = get_print(A1#operation.expression_left, Parser)
            },
            code(Rest0, copy_rowcol(Parser0, Parser), [Print|Parsed]);
        _ ->
            code(Rest0, copy_rowcol(Parser0, Parser), [Call0|Parsed])
    end;
code(<<P:8,R:8,I:8,N:8,T:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(P,$p,$P) andalso ?OR(R,$r,$R) andalso ?OR(I,$i,$I)
        andalso ?OR(N,$n,$N) andalso ?OR(T,$t,$T)
        andalso (?IS_SPACE(SP) orelse SP =:= $() ->
    Call = add_line(#call{name = <<"print">>}, Parser),
    {Rest0, Parser0, [Call0]} =
        ephp_parser_func:echo(<<SP:8,Rest/binary>>, add_pos(Parser, 5), [Call]),
    % FIXME if we detect an OR or AND expression, we put around print
    case Call0#call.args of
        [#operation{type = Type} = A1] when Type =:= 'or' orelse Type =:= 'and' ->
            Print = A1#operation{
                expression_left = get_print(A1#operation.expression_left, Parser)
            },
            code(Rest0, copy_rowcol(Parser0, Parser), [Print|Parsed]);
        _ ->
            code(Rest0, copy_rowcol(Parser0, Parser), [Call0|Parsed])
    end;
code(<<C:8,O:8,N:8,S:8,T:8,SP:8,Rest/binary>>, Parser, Parsed)
        when ?OR(C,$c,$C) andalso ?OR(O,$o,$O) andalso ?OR(N,$n,$N)
        andalso ?OR(S,$s,$S) andalso ?OR(T,$t,$T) andalso ?IS_SPACE(SP) ->
    {Rest0, Parser0, #assign{variable = #constant{} = Const, expression = Value}} =
        expression(Rest, add_pos(Parser, 6), []),
    Constant = Const#constant{type = define, value = Value},
    code(Rest0, copy_rowcol(Parser0, Parser), [Constant|Parsed]);
code(<<F:8,U:8,N:8,C:8,T:8,I:8,O:8,N:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(F,$F,$f) andalso ?OR(U,$U,$u) andalso ?OR(N,$N,$n) andalso
        ?OR(C,$C,$c) andalso ?OR(T,$T,$t) andalso ?OR(I,$I,$i) andalso
        ?OR(O,$O,$o) andalso ?IS_SPACE(SP) ->
    {Rest0, Parser0, [#function{}=Function]} =
        ephp_parser_func:st_function(Rest, add_pos(Parser, 9), []),
    code(Rest0, copy_rowcol(Parser0, Parser), Parsed ++ [Function]);
code(<<F:8,U:8,N:8,C:8,T:8,I:8,O:8,N:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(F,$F,$f) andalso ?OR(U,$U,$u) andalso ?OR(N,$N,$n) andalso
        ?OR(C,$C,$c) andalso ?OR(T,$T,$t) andalso ?OR(I,$I,$i) andalso
        ?OR(O,$O,$o) andalso ?IS_NEWLINE(SP) ->
    {Rest0, Parser0, #function{} = Function} =
        ephp_parser_func:st_function(Rest, new_line(Parser), []),
    code(Rest0, copy_rowcol(Parser0, Parser), Parsed ++ [Function]);
code(<<"?>\n",Rest/binary>>, #parser{level = code_value} = Parser, [Parsed]) ->
    {Rest, new_line(Parser), Parsed};
code(<<"?>",Rest/binary>>, #parser{level = code_value} = Parser, [Parsed]) ->
    {Rest, add_pos(Parser, 2), Parsed};
code(<<"?>\n",Rest/binary>>, #parser{level = L} = Parser, Parsed) when
        L =:= code_block orelse L =:= if_old_block orelse
        L =:= while_old_block orelse L =:= for_old_block orelse
        L =:= foreach_old_block orelse L =:= switch_block orelse
        L =:= switch_old_block ->
    NewParser = new_line(literal_level(Parser)),
    {Rest0, Parser0, Text} = document(Rest, NewParser, []),
    code(Rest0, copy_rowcol(Parser0, Parser), Text ++ Parsed);
code(<<"?>",Rest/binary>>, #parser{level = L} = Parser, Parsed) when
        L =:= code_block orelse L =:= if_old_block orelse
        L =:= while_old_block orelse L =:= for_old_block orelse
        L =:= foreach_old_block orelse L =:= switch_block orelse
        L =:= switch_old_block ->
    {Rest0, Parser0, Text} = document(Rest, literal_level(add_pos(Parser, 2)), []),
    code(Rest0, copy_rowcol(Parser0, Parser), Text ++ Parsed);
code(<<"?>", _/binary>> = Rest, #parser{level = code_statement} = Parser, Parsed) ->
    {Rest, Parser, Parsed};
code(<<"?>\n", Rest/binary>>, Parser, Parsed) ->
    {Rest, new_line(Parser), Parsed};
code(<<"?>", Rest/binary>>, Parser, Parsed) ->
    {Rest, add_pos(Parser, 2), Parsed};
code(<<"//", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_line(Rest, add_pos(Parser, 2), Parsed),
    code(Rest0, Parser0, Parsed);
code(<<"#", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_line(Rest, inc_pos(Parser), Parsed),
    code(Rest0, Parser0, Parsed);
code(<<"/*", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_block(Rest, add_pos(Parser, 2), Parsed),
    code(Rest0, Parser0, Parsed);
code(<<"<<<", _/binary>> = Rest, Parser, Parsed) ->
    {Rest0, Parser0, S} = ephp_parser_string:string(Rest,Parser,[]),
    code(Rest0, copy_rowcol(Parser0, Parser), [S|Parsed]);
code(<<I:8,N:8,C:8,L:8,U:8,D:8,E:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(I,$I,$i) andalso ?OR(N,$N,$n) andalso ?OR(C,$C,$c) andalso
        ?OR(L,$L,$l) andalso ?OR(U,$U,$u) andalso ?OR(D,$D,$d) andalso
        ?OR(E,$E,$e) andalso (?IS_SPACE(SP) orelse SP =:= $() ->
    {Rest0, Parser0} = remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 7)),
    {Rest1, Parser1, Exp} = expression(Rest0, Parser0, []),
    Include = add_line(#call{name = <<"include">>, args = [Exp]}, Parser),
    code(Rest1, Parser1, [Include|Parsed]);
code(<<I:8,N:8,C:8,L:8,U:8,D:8,E:8,$_,O:8,N:8,C:8,E:8,SP:8,Rest/binary>>,
     Parser, Parsed) when
        ?OR(I,$I,$i) andalso ?OR(N,$N,$n) andalso ?OR(C,$C,$c) andalso
        ?OR(L,$L,$l) andalso ?OR(U,$U,$u) andalso ?OR(D,$D,$d) andalso
        ?OR(E,$E,$e) andalso ?OR(O,$O,$o) andalso
        (?IS_SPACE(SP) orelse SP =:= $() ->
    {Rest0, Parser0} = remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 12)),
    {Rest1, Parser1, Exp} = expression(Rest0, Parser0, []),
    Include = add_line(#call{name = <<"include_once">>, args = [Exp]}, Parser),
    code(Rest1, Parser1, [Include|Parsed]);
code(<<R:8,E:8,Q:8,U:8,I:8,R:8,E:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(R,$R,$r) andalso ?OR(E,$E,$e) andalso ?OR(Q,$Q,$q) andalso
        ?OR(U,$U,$u) andalso ?OR(I,$I,$i) andalso
        (?IS_SPACE(SP) orelse SP =:= $() ->
    {Rest0, Parser0} = remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 7)),
    {Rest1, Parser1, Exp} = expression(Rest0, Parser0, []),
    Include = add_line(#call{name = <<"require">>, args = [Exp]}, Parser),
    code(Rest1, Parser1, [Include|Parsed]);
code(<<R:8,E:8,Q:8,U:8,I:8,R:8,E:8,$_,O:8,N:8,C:8,E:8,SP:8,Rest/binary>>,
     Parser, Parsed) when
        ?OR(R,$R,$r) andalso ?OR(E,$E,$e) andalso ?OR(Q,$Q,$q) andalso
        ?OR(U,$U,$u) andalso ?OR(I,$I,$i) andalso ?OR(O,$O,$o) andalso
        ?OR(N,$N,$n) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse SP =:= $() ->
    {Rest0, Parser0} = remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 12)),
    {Rest1, Parser1, Exp} = expression(Rest0, Parser0, []),
    Include = add_line(#call{name = <<"require_once">>, args = [Exp]}, Parser),
    code(Rest1, Parser1, [Include|Parsed]);
code(<<S:8,T:8,A:8,T:8,I:8,C:8,SP:8,Rest/binary>>, Parser, Parsed) when
        ?OR(S,$S,$s) andalso ?OR(T,$T,$t) andalso ?OR(A,$A,$a) andalso
        ?OR(I,$I,$i) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0, Parsed0} = static(Rest, add_pos(Parser, 7), []),
    code(Rest0, copy_rowcol(Parser0, Parser), Parsed0 ++ Parsed);
code(<<A:8,_/binary>> = Rest, Parser, [#constant{}|_])
        when ?IS_ALPHA(A) orelse A =:= $_ orelse A =:= $\\ ->
    throw_error(eparse, Parser, Rest);
code(<<A:8,_/binary>> = Rest, Parser, Parsed) when
        ?IS_ALPHA(A) orelse A =:= $_ orelse A =:= $\\ ->
    {Rest0, Parser0, Parsed0} = expression(Rest, Parser, []),
    code(Rest0, copy_rowcol(Parser0, Parser), [Parsed0] ++ Parsed);
code(<<A:8,_/binary>> = Rest, Parser, Parsed) when ?IS_NUMBER(A)
                                           orelse A =:= $- orelse A =:= $(
                                           orelse A =:= $" orelse A =:= $'
                                           orelse A =:= $$ orelse A =:= $+
                                           orelse A =:= 126 orelse A =:= $! ->
    {Rest0, Parser0, Exp} = expression(Rest, Parser, []),
    code(Rest0, copy_rowcol(Parser0, Parser), [Exp|Parsed]);
code(<<Space:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(Space) ->
    code(Rest, inc_pos(Parser), Parsed);
code(<<NewLine:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(NewLine) ->
    code(Rest, new_line(Parser), Parsed);
code(<<";",Rest/binary>>, Parser, Parsed) ->
    code(Rest, inc_pos(Parser), Parsed);
code(Rest, Parser, [#try_catch{catches = C, finally = F}|_Parsed])
        when C =:= [] andalso F =:= [] ->
    throw_error(enocatch, Parser, Rest);
code(<<>>, Parser, Parsed) ->
    {<<>>, Parser, Parsed};
code(Text, Parser, _Parsed) ->
    throw_error(eparse, Parser, Text).

code_block(<<"//",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_line(Rest, Parser, Parsed),
    code_block(Rest0, Parser0, Parsed);
code_block(<<"#",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_line(Rest, Parser, Parsed),
    code_block(Rest0, Parser0, Parsed);
code_block(<<"/*",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_block(Rest, Parser, Parsed),
    code_block(Rest0, Parser0, Parsed);
%% TODO change this to use always 'code/3' as one statement and block calls
%%      several times until '}' is received. Same for old block but given known
%%      constants instead.
code_block(<<"{", Rest/binary>>, Parser, Parsed) ->
    code(Rest, code_block_level(inc_pos(Parser)), Parsed);
code_block(<<":", Rest/binary>>, #parser{level = if_block} = Parser, Parsed) ->
    code(Rest, if_old_block_level(inc_pos(Parser)), Parsed);
code_block(<<":", Rest/binary>>, #parser{level = foreach_block} = Parser, Parsed) ->
    code(Rest, foreach_old_block_level(inc_pos(Parser)), Parsed);
code_block(<<":", Rest/binary>>, #parser{level = for_block} = Parser, Parsed) ->
    code(Rest, for_old_block_level(inc_pos(Parser)), Parsed);
code_block(<<":", Rest/binary>>, #parser{level = while_block} = Parser, Parsed) ->
    code(Rest, while_old_block_level(inc_pos(Parser)), Parsed);
code_block(<<SP:8, Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    code_block(Rest, inc_pos(Parser), Parsed);
code_block(<<SP:8, Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    code_block(Rest, new_line(Parser), Parsed);
code_block(<<>>, Parser, Parsed) ->
    {<<>>, Parser, Parsed};
code_block(Rest, Parser, Parsed) ->
    code(Rest, code_statement_level(Parser), Parsed).

get_ns([], #parser{namespace = BaseNS}) -> BaseNS;
get_ns(NS, #parser{use_list = UseList, namespace = BaseNS}) ->
    FullNS = ephp_ns:join(BaseNS, NS),
    case lists:keyfind(FullNS, 1, UseList) of
        {NS, RealNS} -> RealNS;
        false -> FullNS
    end.

use_const(Rest, #parser{use_const_list = ConstList,
                        use_ns = UseNS} = Parser, Parsed) ->
    {Rest0, Parser0, NameSpace} = namespace(Rest, Parser, []),
    {NS, BaseName} = ephp_ns:split(NameSpace),
    BaseNS = ephp_ns:join(UseNS, NS),
    case remove_spaces(Rest0, Parser0) of
        {<<"{", Rest1/binary>>, #parser{use_ns = []} = Parser1} ->
            RealBaseNS = ephp_ns:join(BaseNS, BaseName),
            use_const(Rest1, inc_pos(Parser1#parser{use_ns = RealBaseNS}), Parsed);
        {<<"}", Rest1/binary>>, #parser{use_ns = UseNS} = Parser1}
                when UseNS =/= [] ->
            use_const(Rest1, inc_pos(Parser1#parser{use_ns = []}), Parsed);
        %% TODO
        % {<<$;, _/binary>>, Parser1} when UseNS =/= [] ->
        %     throw_error(...);
        % {<<$,, _/binary>>, #parser{use_ns = []} = Parser1} ->
        %     throw_error(...);
        {<<SEP:8, Rest1/binary>>, Parser1} when ?OR(SEP, $;, $,) ->
            NewUseConstList = [{BaseName, BaseNS}|ConstList],
            Parser2 = inc_pos(Parser1#parser{use_const_list = NewUseConstList}),
            case SEP of
                $; -> code(Rest1, Parser2, Parsed);
                $, -> use_const(Rest1, Parser2, Parsed)
            end
    end.

use_function(Rest, #parser{use_func_list = FuncList,
                           use_ns = UseNS} = Parser, Parsed) ->
    {Rest0, Parser0, NameSpace} = namespace(Rest, Parser, []),
    {NS, BaseName} = ephp_ns:split(NameSpace),
    BaseNS = ephp_ns:join(UseNS, NS),
    case remove_spaces(Rest0, Parser0) of
        {<<"{", Rest1/binary>>, #parser{use_ns = []} = Parser1} ->
            RealBaseNS = ephp_ns:join(BaseNS, BaseName),
            use_function(Rest1, inc_pos(Parser1#parser{use_ns = RealBaseNS}), Parsed);
        {<<"}", Rest1/binary>>, #parser{use_ns = UseNS} = Parser1}
                when UseNS =/= [] ->
            use_function(Rest1, inc_pos(Parser1#parser{use_ns = []}), Parsed);
        %% TODO
        % {<<$;, _/binary>>, Parser1} when UseNS =/= [] ->
        %     throw_error(...);
        % {<<$,, _/binary>>, #parser{use_ns = []} = Parser1} ->
        %     throw_error(...);
        {<<SEP:8, Rest1/binary>>, Parser1} when ?OR(SEP, $;, $,) ->
            NewUseConstList = [{BaseName, BaseNS}|FuncList],
            Parser2 = inc_pos(Parser1#parser{use_func_list = NewUseConstList}),
            case SEP of
                $; -> code(Rest1, Parser2, Parsed);
                $, -> use_function(Rest1, Parser2, Parsed)
            end
    end.

use_list(Rest, #parser{use_ns = UseNS, use_list = UseList} = Parser, Parsed) ->
    {Rest0, Parser0, NameSpace} = namespace(Rest, Parser, []),
    {NS, Name} = ephp_ns:split(NameSpace),
    BaseNS = ephp_ns:join(UseNS, NS ++ [Name]),
    case remove_spaces(Rest0, Parser0) of
        {<<"{", Rest1/binary>>, #parser{use_ns = []} = Parser1} ->
            use_list(Rest1, inc_pos(Parser1#parser{use_ns = BaseNS}), Parsed);
        {<<"}", Rest1/binary>>, #parser{use_ns = UseNS} = Parser1}
                when UseNS =/= [] ->
            use_list(Rest1, inc_pos(Parser1#parser{use_ns = []}), Parsed);
        %% TODO
        % {<<$;, _/binary>>, Parser1} when UseNS =/= [] ->
        %     throw_error(...);
        % {<<$,, _/binary>>, #parser{use_ns = []} = Parser1} ->
        %     throw_error(...);
        {<<SEP:8, Rest1/binary>>, Parser1} when ?OR(SEP, $;, $,) ->
            AliasNS = [lists:last(BaseNS)],
            NewUseList = [{AliasNS, BaseNS}|UseList],
            Parser2 = inc_pos(Parser1#parser{use_list = NewUseList}),
            case SEP of
                $; -> code(Rest1, Parser2, Parsed);
                $, -> use_list(Rest1, Parser2, Parsed)
            end;
        {<<A:8,S:8,SP:8,Rest1/binary>>, Parser1} when
                ?OR(A,$A,$a) andalso ?OR(S,$S,$s) andalso
                (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
            {Rest2, Parser2, AliasNS} = namespace(<<SP:8, Rest1/binary>>,
                                                  add_pos(Parser1, 2), []),
            NewUseList = [{AliasNS, BaseNS}|UseList],
            code(Rest2, Parser2#parser{use_list = NewUseList}, Parsed)
    end.

namespace(<<SP:8, Rest/binary>>, Parser, []) when ?IS_SPACE(SP) ->
    namespace(Rest, inc_pos(Parser), []);
namespace(<<SP:8, Rest/binary>>, Parser, []) when ?IS_NEWLINE(SP) ->
    namespace(Rest, new_line(Parser), []);
namespace(<<"\\", Rest/binary>>, Parser, Parsed) ->
    namespace(Rest, inc_pos(Parser), [<<>>|Parsed]);
namespace(<<A:8, _/binary>> = Rest, Parser, []) when
        ?IS_ALPHANUM(A) orelse A =:= $_ ->
    namespace(Rest, Parser, [<<>>]);
namespace(<<A:8, Rest/binary>>, Parser, [NamePart|Parsed]) when
        ?IS_ALPHANUM(A) orelse A =:= $_ ->
    namespace(Rest, inc_pos(Parser), [<<NamePart/binary, A:8>>|Parsed]);
namespace(Rest, Parser, Parsed) ->
    {Rest, Parser, lists:reverse(Parsed)}.

static(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    static(Rest, inc_pos(Parser), Parsed);
static(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    static(Rest, new_line(Parser), Parsed);
static(<<",",Rest/binary>>, Parser, Parsed) when Parsed =/= [] ->
    static(Rest, inc_pos(Parser), Parsed);
static(<<",", _/binary>>, Parser, _Parsed) ->
    throw_error(eparse, Parser, <<",">>);
static(<<";", _/binary>> = Rest, Parser, Parsed) ->
    {Rest, Parser, Parsed};
static(<<>>, Parser, _Parsed) ->
    throw_error(eparse, Parser, <<>>);
static(Rest, Parser, Parsed) ->
    case expression(Rest, arg_level(Parser), []) of
        {Rest0, Parser0, #assign{variable = Var} = Assign} ->
            NewAssign = Assign#assign{variable = Var#variable{type = static}},
            static(Rest0, copy_rowcol(Parser0, Parser), Parsed ++ [NewAssign]);
        {Rest0, Parser0, #variable{} = Var} ->
            NewVar = Var#variable{type = static},
            static(Rest0, copy_rowcol(Parser0, Parser), Parsed ++ [NewVar])
    end.

variable(<<SP:8,Rest/binary>>, Parser, []) when ?IS_SPACE(SP) ->
    variable(Rest, inc_pos(Parser), []);
variable(<<SP:8,Rest/binary>>, #parser{level = enclosed} = Parser, Var) when ?IS_SPACE(SP) ->
    variable(Rest, inc_pos(Parser), Var);
variable(<<SP:8,Rest/binary>>, Parser, []) when ?IS_NEWLINE(SP) ->
    variable(Rest, new_line(Parser), []);
variable(<<SP:8,Rest/binary>>, #parser{level = enclosed} = Parser, Var) when ?IS_NEWLINE(SP) ->
    variable(Rest, new_line(Parser), Var);
variable(<<"$",Rest/binary>>, Parser, []) ->
    variable(Rest, inc_pos(Parser), []);
variable(<<A:8,Rest/binary>>, Parser, [])
        when ?IS_ALPHA(A) orelse A =:= $_ orelse A >= 16#7f ->
    Var = add_line(#variable{name = <<A:8>>}, Parser),
    variable(Rest, inc_pos(Parser), [Var]);
variable(<<A:8, Rest/binary>>, #parser{} = Parser, [#variable{name = N} = V])
        when ?IS_NUMBER(A) orelse ?IS_ALPHA(A) orelse A =:= $_
        orelse A >= 16#7f ->
    variable(Rest, inc_pos(Parser), [V#variable{name = <<N/binary, A:8>>}]);
variable(<<SP:8, _/binary>> = Rest, #parser{level = unclosed} = Parser, Var)
        when ?IS_SPACE(SP) ->
    {Rest, inc_pos(Parser), Var};
variable(<<SP:8, _/binary>> = Rest, #parser{level = unclosed} = Parser, Var)
        when ?IS_NEWLINE(SP) ->
    {Rest, new_line(Parser), Var};
variable(Rest, Parser, []) ->
    throw_error(eparse, Parser, Rest);
variable(Rest, Parser, Var) ->
    var_access(Rest, Parser, Var).

var_access(<<"}", _/binary>> = Rest, #parser{level = enclosed} = Parser, Var) ->
    {Rest, Parser, Var};
var_access(<<"[", Rest/binary>>, Parser, [#variable{idx = Indexes} = Var]) ->
    {Rest1, Parser1, RawIdx} = expression(Rest, array_level(inc_pos(Parser)), []),
    Idx = case RawIdx of
        [] -> auto;
        _ -> RawIdx
    end,
    NewVar = Var#variable{idx = Indexes ++ [Idx]},
    var_access(Rest1, copy_rowcol(Parser1, Parser), [NewVar]);
var_access(<<"{", Rest/binary>>, Parser, [#variable{idx = Indexes} = Var]) ->
    NewParser = array_curly_level(inc_pos(Parser)),
    {Rest1, Parser1, RawIdx} = expression(Rest, NewParser, []),
    Idx = case RawIdx of
        [] -> auto;
        _ -> RawIdx
    end,
    NewVar = Var#variable{idx = Indexes ++ [Idx]},
    var_access(Rest1, copy_rowcol(Parser1, Parser), [NewVar]);
var_access(<<"->",Rest/binary>>, #parser{level = L} = Parser, [#variable{} = Var])
        when is_number(L) ->
    % TODO move this code to ephp_parser_expr
    OpL = <<"->">>,
    Op = add_op({OpL, precedence(OpL), Parser}, add_op(Var, [])),
    {Rest0, Parser0, [Exp]} = accessor(Rest, arg_level(add_pos(Parser, 2)), []),
    var_access(Rest0, copy_rowcol(Parser0, Parser), [add_op('end', add_op(Exp, Op))]);
var_access(<<"->",Rest/binary>>, Parser, [#variable{} = Var]) ->
    % TODO move this code to ephp_parser_expr
    OpL = <<"->">>,
    Op = add_op({OpL, precedence(OpL), Parser}, add_op(Var, [])),
    {Rest0, Parser0, [Exp]} = accessor(Rest, add_pos(Parser, 2), []),
    var_access(Rest0, copy_rowcol(Parser0, Parser), [add_op('end', add_op(Exp, Op))]);
var_access(Rest, Parser, Parsed) ->
    {Rest, Parser, Parsed}.

accessor(<<A:8, Rest/binary>>, Parser, Parsed) when ?IS_SPACE(A) ->
    accessor(Rest, inc_pos(Parser), Parsed);
accessor(<<A:8, Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(A) ->
    accessor(Rest, new_line(Parser), Parsed);
accessor(<<A:8, _/binary>> = Rest, Parser, []) when ?IS_ALPHA(A) orelse A =:= $_ ->
    constant(Rest, Parser, []);
accessor(<<"$", Rest/binary>>, Parser, []) ->
    variable(Rest, Parser, []);
accessor(<<"{", Rest/binary>>, Parser, []) ->
    NewParser = inc_pos(enclosed_level(Parser)),
    {Rest0, Parser0, Acc} = expression(Rest, NewParser, []),
    case remove_spaces(Rest0, Parser0) of
        {<<"(", Rest1/binary>>, Parser1} ->
            NParser1 = inc_pos(Parser1),
            {Rest2, Parser2, Args} = ephp_parser_func:call_args(Rest1, NParser1, []),
            {Rest2, copy_rowcol(Parser2, Parser),
             [add_line(#call{name = Acc, args = Args,
                             namespace = Parser#parser.namespace}, Parser1)]};
        {Rest1, Parser1} ->
            {Rest1, copy_rowcol(Parser1, Parser), [Acc]}
    end;
accessor(<<>>, Parser, Parsed) ->
    {<<>>, Parser, Parsed};
accessor(_Rest, Parser, []) ->
    throw_error(eparse, Parser, {<<"`\"identifier (T_STRING)\"' or "
                                "`\"variable (T_VARIABLE)\"' or "
                                "`'{'' or `'$''">>}).


constant(<<A:8,Rest/binary>>, Parser, [])
        when (?IS_ALPHA(A) orelse ?IS_NUMBER(A) orelse A =:= $_ orelse A =:= $\\)
        andalso Parser#parser.level =/= unclosed ->
    constant(Rest, inc_pos(Parser), [add_line(#constant{name = <<A:8>>}, Parser)]);

constant(<<A:8,Rest/binary>>, Parser, []) when ?IS_ALPHA(A) orelse A =:= $_ ->
    constant(Rest, inc_pos(Parser), [add_line(#constant{name = <<A:8>>}, Parser)]);

constant(<<A:8,Rest/binary>>, Parser, [#constant{name = N} = C])
        when (?IS_ALPHA(A) orelse ?IS_NUMBER(A) orelse A =:= $_ orelse A =:= $\\)
        andalso Parser#parser.level =/= unclosed ->
    constant(Rest, inc_pos(Parser), [C#constant{name = <<N/binary, A:8>>}]);

constant(<<A:8,Rest/binary>>, Parser, [#constant{name = N} = C])
        when ?IS_ALPHA(A) orelse ?IS_NUMBER(A) orelse A =:= $_ ->
    constant(Rest, inc_pos(Parser), [C#constant{name = <<N/binary, A:8>>}]);

constant(<<SP:8,_/binary>> = Rest, #parser{level = unclosed} = Parser, [#constant{}] = Parsed)
        when ?IS_SPACE(SP) ->
    {Rest, Parser, Parsed};

constant(<<SP:8,Rest/binary>>, Parser, [#constant{}] = Parsed)
        when ?IS_SPACE(SP) ->
    constant_wait(Rest, inc_pos(Parser), Parsed);

constant(<<SP:8,_/binary>> = Rest, #parser{level = unclosed} = Parser, [#constant{}] =  Parsed)
        when ?IS_NEWLINE(SP) ->
    {Rest, Parser, Parsed};

constant(<<SP:8,Rest/binary>>, Parser, [#constant{}] = Parsed)
        when ?IS_NEWLINE(SP) ->
    constant_wait(Rest, new_line(Parser), Parsed);

constant(<<"(",_/binary>> = Rest, Parser, Parsed) ->
    constant_wait(Rest, Parser, Parsed);
% TODO fail when unclosed is used?

constant(<<"::",_/binary>> = Rest, Parser, Parsed) ->
    constant_wait(Rest, Parser, Parsed);

constant(Rest, Parser, Parsed) ->
    {Rest, Parser, constant_known(Parsed, Parser)}.

%% if after one or several spaces there are a parens, it's a function
%% but if not, it should returns
constant_wait(<<"(", Rest/binary>>, Parser, [#constant{} = C]) ->
    {NS, Name} = ephp_parser_func:get_ns(ephp_ns:parse(C#constant.name), Parser),
    Call = #call{name = Name, line = C#constant.line, namespace = NS},
    ephp_parser_func:function(Rest, inc_pos(Parser), [Call]);
constant_wait(<<"::$", Rest/binary>>, Parser, [#constant{} = C]) ->
    NewParser = arg_level(add_pos(Parser, 2)),
    {Rest1, Parser1, [Var]} = variable(<<"$", Rest/binary>>, NewParser, []),
    {NS, RealClassName} = ephp_ns:parse(C#constant.name),
    NewVar = Var#variable{type = class, class = RealClassName, class_ns = NS},
    {Rest1, copy_rowcol(Parser1, Parser), [NewVar]};
constant_wait(<<"::",Rest/binary>>, Parser, [#constant{} = Cons]) ->
    case constant(Rest, add_pos(Parser, 2), []) of
        {Rest1, Parser1, [#constant{name = <<"class">>}]} ->
            {Rest1, Parser1, [add_line(#text{text = Cons#constant.name}, Parser)]};
        {Rest1, Parser1, [#constant{} = C]} ->
            {Rest1, Parser1, [C#constant{type=class, class = Cons#constant.name}]};
        {Rest1, Parser1, [#call{} = C]} ->
            {Rest1, Parser1, [C#call{type = class, class = Cons#constant.name}]}
    end;
constant_wait(<<SP:8,Rest/binary>>, Parser, [#constant{}] = Parsed)
        when ?IS_SPACE(SP) ->
    constant_wait(Rest, inc_pos(Parser), Parsed);
constant_wait(<<SP:8,Rest/binary>>, Parser, [#constant{}] = Parsed)
        when ?IS_NEWLINE(SP) ->
    constant_wait(Rest, new_line(Parser), Parsed);
constant_wait(Rest, Parser, Parsed) ->
    {Rest, Parser, constant_known(Parsed, Parser)}.

constant_known([#constant{name = <<"__LINE__">>}|Parsed],
               #parser{row = R} = Parser) ->
    [add_line(#int{int = R}, Parser)|Parsed];
constant_known([#constant{name = <<"__NAMESPACE__">>}|Parsed],
               #parser{namespace = NS} = Parser) ->
    [add_line(#text{text = ephp_string:join(NS, <<"\\">>)}, Parser)|Parsed];
constant_known([#constant{name = <<"exit">>}|Parsed], Parser) ->
    [add_line(#call{name = <<"exit">>}, Parser)|Parsed];
constant_known([#constant{name = RawName} = C|Parsed], Parser) ->
    {NS, Name} = ephp_ns:parse(RawName),
    case lists:member(C#constant.name, ephp_const:special_consts()) of
        true -> [C|Parsed];
        false -> [C#constant{namespace = ephp_ns:join(Parser#parser.namespace, NS),
                             name = Name}|Parsed]
    end.

st_global(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    st_global(Rest, inc_pos(Parser), Parsed);
st_global(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    st_global(Rest, new_line(Parser), Parsed);
st_global(<<",",Rest/binary>>, Parser, Parsed) ->
    st_global(Rest, inc_pos(Parser), Parsed);
st_global(<<";", _/binary>> = Rest, Parser, Parsed) ->
    Global = add_line(#global{vars = Parsed}, Parser),
    {Rest, Parser, [Global]};
st_global(<<"$",_/binary>> = Rest, Parser, Parsed) ->
    {Rest0, Parser0, [Var]} = variable(Rest, Parser, []),
    st_global(Rest0, Parser0, [Var|Parsed]).

st_while(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    st_while(Rest, inc_pos(Parser), Parsed);
st_while(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    st_while(Rest, new_line(Parser), Parsed);
st_while(<<"(",Rest/binary>>, Parser, Parsed) ->
    NewParser = inc_pos(Parser),
    {<<")",Rest1/binary>>, Parser1, Conditions} =
        expression(Rest, arg_level(NewParser), []),
    {Rest2, Parser2, CodeBlock} = code_block(Rest1, while_block_level(Parser1), []),
    While = add_line(#while{type = pre,
                            conditions = Conditions,
                            loop_block = CodeBlock}, Parser),
    {Rest2, copy_rowcol(Parser2, Parser), [While|Parsed]};
st_while(<<>>, Parser, _Parsed) ->
    throw_error(eparse, Parser, <<>>).

st_do_while(Rest, Parser, Parsed) ->
    case code_block(Rest, Parser, []) of
        {<<";", Rest0/binary>>, Parser0, CodeBlock} -> ok;
        {Rest0, Parser0, CodeBlock} -> ok
    end,
    {<<WhileRaw:5/binary,Rest1/binary>>, Parser1} = remove_spaces(Rest0, Parser0),
    <<"while">> = ephp_string:to_lower(WhileRaw),
    {Rest2, Parser2, [While]} = st_while(Rest1, Parser1, []),
    DoWhile = add_line(While#while{type = post,
                                   loop_block = CodeBlock}, Parser),
    {Rest2, copy_rowcol(Parser2, Parser), [DoWhile|Parsed]}.

st_if(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    st_if(Rest, inc_pos(Parser), Parsed);
st_if(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    st_if(Rest, new_line(Parser), Parsed);
st_if(<<";",Rest/binary>>, Parser, [#if_block{}] = Parsed) ->
    st_if(Rest, inc_pos(Parser), Parsed);
st_if(<<"//",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_line(Rest, Parser, Parsed),
    st_if(Rest0, Parser0, Parsed);
st_if(<<"#",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_line(Rest, Parser, Parsed),
    st_if(Rest0, Parser0, Parsed);
st_if(<<"/*",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = comment_block(Rest, Parser, Parsed),
    st_if(Rest0, Parser0, Parsed);
st_if(<<"(",Rest/binary>>, Parser, []) ->
    NewParser = inc_pos(Parser),
    {<<")",Rest1/binary>>, Parser1, Conditions} =
        expression(Rest, arg_level(NewParser), []),
    {Rest2, Parser2, CodeBlock} = code_block(Rest1, if_block_level(Parser1), []),
    If = add_line(#if_block{conditions = Conditions,
                            true_block = CodeBlock}, Parser),
    st_if(Rest2, copy_rowcol(Parser2, Parser), [If]);
st_if(<<E:8,L:8,S:8,E:8,SP:8,Rest/binary>>, Parser, [#if_block{} = If|Parsed]) when
        ?OR(E,$e,$E) andalso ?OR(L,$l,$L) andalso ?OR(S,$s,$S) andalso
        (?OR(SP,${,$:) orelse ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8,Rest/binary>>, add_pos(Parser, 4)),
    BlockParser = if_block_level(Parser0),
    {Rest1, Parser1, CodeBlock} = code_block(Rest0, BlockParser, []),
    IfWithElse = If#if_block{false_block = CodeBlock},
    {Rest1, copy_rowcol(Parser1, Parser), [IfWithElse|Parsed]};
st_if(<<E:8,L:8,S:8,E:8,I:8,F:8,SP:8,Rest/binary>>, Parser,
      [#if_block{} = If|Parsed]) when
        ?OR(E,$e,$E) andalso ?OR(L,$l,$L) andalso ?OR(S,$s,$S) andalso
        ?OR(I,$i,$I) andalso ?OR(F,$f,$F) andalso
        (SP =:= $( orelse ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest1, Parser1, CodeBlock} = st_if(<<SP:8,Rest/binary>>, add_pos(Parser, 6), []),
    IfWithElse = If#if_block{false_block = CodeBlock},
    {Rest1, copy_rowcol(Parser1, Parser), [IfWithElse|Parsed]};
st_if(<<>>, Parser, [#if_block{}] = Parsed) ->
    {<<>>, Parser, Parsed};
st_if(<<>>, Parser, _Parsed) ->
    throw_error(eparse, Parser, <<>>);
st_if(Rest, Parser, [#if_block{}] = Parsed) ->
    {Rest, Parser, Parsed}.

args(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    args(Rest, inc_pos(Parser), Parsed);
args(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    args(Rest, new_line(Parser), Parsed);
args(Rest, Parser, Args) when Rest =/= <<>> ->
    case expression(Rest, arg_level(Parser), []) of
        {<<")",_/binary>> = Rest0, Parser0, Arg} ->
            {Rest0, inc_pos(Parser0), Args ++ [Arg]};
        {<<";",_/binary>> = Rest0, Parser0, Arg} ->
            {Rest0, inc_pos(Parser0), Args ++ [Arg]};
        {<<",", Rest0/binary>>, Parser0, Arg} ->
            args(Rest0, inc_pos(Parser0), Args ++ [Arg]);
        {Rest0, Parser0, Arg} ->
            args(Rest0, Parser0, Args ++ [Arg])
    end.

st_foreach(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    st_foreach(Rest, inc_pos(Parser), Parsed);
st_foreach(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    st_foreach(Rest, new_line(Parser), Parsed);
st_foreach(<<"(",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, Exp} = expression(Rest, foreach_block_level(Parser), []),
    {<<AS:2/binary,Rest1/binary>>, Parser1} = remove_spaces(Rest0, Parser0),
    <<"as">> = ephp_string:to_lower(AS),
    NewParser = array_def_level(add_pos(Parser1, 2)),
    {<<")",Rest2/binary>>, Parser2, ExpIter} = expression(Rest1, NewParser, []),
    BlockParser = foreach_block_level(inc_pos(Parser2)),
    {Rest3, Parser3, CodeBlock} = code_block(Rest2, BlockParser, []),
    RawFor = add_line(#foreach{iter = ExpIter,
                               elements = Exp,
                               loop_block = CodeBlock}, Parser),
    For = case ExpIter of
        #ref{} ->
            RawFor;
        #variable{} ->
            RawFor;
        [KIter,Iter] ->
            RawFor#foreach{kiter = KIter, iter = Iter}
    end,
    {Rest3, copy_rowcol(Parser3, Parser), [For|Parsed]}.

switch_case_block([]) ->
    [];
switch_case_block(Blocks) ->
    {Block, [Switch|Rest]} = lists:splitwith(fun
        (#switch_case{}) -> false;
        (_) -> true
    end, Blocks),
    [Switch#switch_case{code_block = lists:reverse(Block)}|Rest].

st_switch(<<"(",Rest/binary>>, Parser, Parsed) ->
    {<<")", Rest0/binary>>, Parser0, Cond} = expression(Rest, inc_pos(Parser), []),
    NewParser = case remove_spaces(Rest0, inc_pos(Parser0)) of
        {<<"{", Rest1/binary>>, Parser1} ->
            switch_block_level(inc_pos(Parser1));
        {<<":", Rest1/binary>>, Parser1} ->
            switch_old_block_level(inc_pos(Parser1))
    end,
    {Rest2, Parser2, CodeBlock} = code(Rest1, NewParser, []),
    Switch = add_line(#switch{condition = Cond, cases = CodeBlock}, Parser),
    {Rest2, copy_rowcol(Parser2, Parser), [Switch|Parsed]}.

st_for(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    st_for(Rest, inc_pos(Parser), Parsed);
st_for(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    st_for(Rest, new_line(Parser), Parsed);
st_for(<<"(",Rest/binary>>, Parser, Parsed) ->
    {<<";",Rest0/binary>>, Parser0, Init} = args(Rest, inc_pos(Parser), []),
    {<<";",Rest1/binary>>, Parser1, [Cond]} = args(Rest0, inc_pos(Parser0), []),
    {<<")",Rest2/binary>>, Parser2, Upda} = args(Rest1, inc_pos(Parser1), []),
    {Rest3, Parser3, CodeBlock} = code_block(Rest2,
                                             for_block_level(inc_pos(Parser2)), []),
    For = add_line(#for{init = Init,
                        conditions = Cond,
                        update = Upda,
                        loop_block = CodeBlock}, Parser),
    {Rest3, copy_rowcol(Parser3, Parser), [For|Parsed]}.

comment_line(<<>>, Parser, Parsed) ->
    {<<>>, Parser, Parsed};
comment_line(<<"?>", _/binary>> = Rest, Parser, Parsed) ->
    {Rest, Parser, Parsed};
comment_line(<<A:8, Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(A) ->
    {Rest, new_line(Parser), Parsed};
comment_line(<<_/utf8, Rest/binary>>, Parser, Parsed) ->
    comment_line(Rest, inc_pos(Parser), Parsed).

comment_block(<<>>, Parser, _Parsed) ->
    %% TODO: throw parse error
    throw_error(eparse, Parser, missing_comment_end);
comment_block(<<"*/", Rest/binary>>, Parser, Parsed) ->
    {Rest, add_pos(Parser, 2), Parsed};
comment_block(<<A:8, Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(A) ->
    comment_block(Rest, new_line(Parser), Parsed);
comment_block(<<_/utf8, Rest/binary>>, Parser, Parsed) ->
    comment_block(Rest, inc_pos(Parser), Parsed).

%%------------------------------------------------------------------------------
%% helper functions
%%------------------------------------------------------------------------------

copy_rowcol(#parser{row = Row, col = Col}, Parser) ->
    Parser#parser{row = Row, col = Col}.
set_level(Level, Parser) -> Parser#parser{level = Level}.

add_to_text(L, _Parser, [#print_text{text=Text}=P|Parsed]) ->
    NewText = <<Text/binary, L/binary>>,
    [P#print_text{text=NewText}|Parsed];
add_to_text(L, Parser, Parsed) ->
    [add_line(#print_text{text=L}, Parser)|Parsed].

inc_pos(Parser) ->
    add_pos(Parser, 1).

add_pos(#parser{col = Col} = Parser, Offset) ->
    Parser#parser{col = Col + Offset}.

new_line(#parser{row = Row} = Parser, N) ->
    Parser#parser{row = Row + N, col = 1}.

new_line(Parser) ->
    new_line(Parser, 1).

if_old_block_level(Parser) -> set_level(if_old_block, Parser).
for_old_block_level(Parser) -> set_level(for_old_block, Parser).
foreach_old_block_level(Parser) -> set_level(foreach_old_block, Parser).
while_old_block_level(Parser) -> set_level(while_old_block, Parser).
switch_old_block_level(Parser) -> set_level(switch_old_block, Parser).

if_block_level(Parser) -> set_level(if_block, Parser).
for_block_level(Parser) -> set_level(for_block, Parser).
foreach_block_level(Parser) -> set_level(foreach_block, Parser).
while_block_level(Parser) -> set_level(while_block, Parser).
switch_block_level(Parser) -> set_level(switch_block, Parser).
switch_label_level(Parser) -> set_level(switch_label, Parser).

normal_level(Parser) -> set_level(code, Parser).
code_block_level(Parser) -> set_level(code_block, Parser).
code_value_level(Parser) -> set_level(code_value, Parser).
code_statement_level(Parser) -> set_level(code_statement, Parser).
arg_level(Parser) -> set_level(arg, Parser).
array_level(Parser) -> set_level(array, Parser).
array_curly_level(Parser) -> set_level(array_curly, Parser).
array_def_level(Parser) -> (set_level(array_def, Parser))#parser{array_type = old}.
literal_level(Parser) -> set_level(literal, Parser).

enclosed_level(Parser) -> set_level(enclosed, Parser).
unclosed_level(Parser) -> set_level(unclosed, Parser).

get_line(#parser{row = Row, col = Col}) -> {{line, Row}, {column, Col}};
get_line(#array{line = Line}) -> Line;
get_line(#eval{line = Line}) -> Line;
get_line(#print{line = Line}) -> Line;
get_line(#print_text{line = Line}) -> Line;
get_line(#variable{line = Line}) -> Line;
get_line(#constant{line = Line}) -> Line;
get_line(#int{line = Line}) -> Line;
get_line(#float{line = Line}) -> Line;
get_line(#text_to_process{line = Line}) -> Line;
get_line(#text{line = Line}) -> Line;
get_line(#if_block{line = Line}) -> Line;
get_line(#assign{line = Line}) -> Line;
get_line(#array_element{line = Line}) -> Line;
get_line(#for{line = Line}) -> Line;
get_line(#foreach{line = Line}) -> Line;
get_line(#operation{line = Line}) -> Line;
get_line(#concat{line = Line}) -> Line;
get_line(#while{line = Line}) -> Line;
get_line(#return{line = Line}) -> Line;
get_line(#function{line = Line}) -> Line;
get_line(#global{line = Line}) -> Line;
get_line(#ref{line = Line}) -> Line;
get_line(#switch{line = Line}) -> Line;
get_line(#switch_case{line = Line}) -> Line;
get_line(#call{line = Line}) -> Line;
get_line(#class{line = Line}) -> Line;
get_line(#class_method{line = Line}) -> Line;
get_line(#class_const{line = Line}) -> Line;
get_line(#class_attr{line = Line}) -> Line;
get_line(#instance{line = Line}) -> Line;
get_line(#cast{line = Line}) -> Line;
get_line(#throw{line = Line}) -> Line;
get_line(#try_catch{line = Line}) -> Line;
get_line(#catch_block{line = Line}) -> Line;
get_line(#clone{line = Line}) -> Line;
get_line(#command{line = Line}) -> Line.

add_line(true, _) -> true;
add_line(false, _) -> false;
add_line(X, {{line,Row},{column,Col}}) -> add_line(X, #parser{row = Row, col = Col});
add_line(#array{}=A, Parser) -> A#array{line = get_line(Parser)};
add_line(#eval{}=E, Parser) -> E#eval{line = get_line(Parser)};
add_line(#print{}=P, Parser) -> P#print{line = get_line(Parser)};
add_line(#print_text{}=P, Parser) -> P#print_text{line = get_line(Parser)};
add_line(#variable{}=V, Parser) -> V#variable{line = get_line(Parser)};
add_line(#constant{}=O, Parser) -> O#constant{line = get_line(Parser)};
add_line(#int{}=I, Parser) -> I#int{line = get_line(Parser)};
add_line(#float{}=F, Parser) -> F#float{line = get_line(Parser)};
add_line(#text_to_process{}=T, Parser) ->
    T#text_to_process{line = get_line(Parser)};
add_line(#text{}=T, Parser) -> T#text{line = get_line(Parser)};
add_line(#if_block{}=I, Parser) -> I#if_block{line = get_line(Parser)};
add_line(#assign{}=A, Parser) -> A#assign{line = get_line(Parser)};
add_line(#array_element{}=A, Parser) ->
    A#array_element{line = get_line(Parser)};
add_line(#for{}=F, Parser) -> F#for{line = get_line(Parser)};
add_line(#foreach{}=F, Parser) -> F#foreach{line = get_line(Parser)};
add_line(#operation{}=O, Parser) -> O#operation{line = get_line(Parser)};
add_line(#concat{}=O, Parser) -> O#concat{line = get_line(Parser)};
add_line(#while{}=W, Parser) -> W#while{line = get_line(Parser)};
add_line(#return{}=Rt, Parser) -> Rt#return{line = get_line(Parser)};
add_line(#function{}=F, Parser) -> F#function{line = get_line(Parser)};
add_line(#global{}=G, Parser) -> G#global{line = get_line(Parser)};
add_line(#ref{}=Rf, Parser) -> Rf#ref{line = get_line(Parser)};
add_line(#switch{}=S, Parser) -> S#switch{line = get_line(Parser)};
add_line(#switch_case{}=S, Parser) -> S#switch_case{line = get_line(Parser)};
add_line(#call{}=Cl, Parser) -> Cl#call{line = get_line(Parser)};
add_line(#class{}=Cl, Parser) -> Cl#class{line = get_line(Parser)};
add_line(#class_method{}=CM, Parser) ->
    CM#class_method{line = get_line(Parser)};
add_line(#class_const{}=CC, Parser) ->
    CC#class_const{line = get_line(Parser)};
add_line(#class_attr{}=CA, Parser) ->
    CA#class_attr{line = get_line(Parser)};
add_line({object, Expr}, Parser) -> {object, Expr, get_line(Parser)};
add_line({class, Expr}, Parser) -> {class, Expr, get_line(Parser)};
add_line(#instance{}=I, Parser) -> I#instance{line = get_line(Parser)};
add_line(#cast{}=Cs, Parser) -> Cs#cast{line = get_line(Parser)};
add_line(#throw{}=T, Parser) -> T#throw{line = get_line(Parser)};
add_line(#try_catch{}=T, Parser) -> T#try_catch{line = get_line(Parser)};
add_line(#catch_block{}=B, Parser) -> B#catch_block{line = get_line(Parser)};
add_line(#clone{}=L, Parser) -> L#clone{line = get_line(Parser)};
add_line(#command{}=Co, Parser) -> Co#command{line = get_line(Parser)}.

remove_spaces(<<SP:8, Rest/binary>>, Parser) when ?IS_SPACE(SP) ->
    remove_spaces(Rest, inc_pos(Parser));
remove_spaces(<<SP:8, Rest/binary>>, Parser) when ?IS_NEWLINE(SP) ->
    remove_spaces(Rest, new_line(Parser));
remove_spaces(<<>>, Parser) -> {<<>>, Parser};
remove_spaces(Rest, Parser) -> {Rest, Parser}.

get_print({Type, Value, _}, Parser) when
        Type =:= int; Type =:= float; Type =:= text ->
    add_line(#print_text{text = ephp_data:to_bin(Value)}, Parser);
get_print(Value, Parser) when is_atom(Value) ->
    add_line(#print_text{text = ephp_data:to_bin(Value)}, Parser);
get_print(Expr, Parser) ->
    add_line(#print{expression = Expr}, Parser).

throw_error(Error, #parser{row = Row, col = Col}, ErrorLevel, Data) ->
    Index = {{line, Row}, {column, Col}},
    ephp_error:error({error, Error, Index, ErrorLevel, Data}).

throw_error(Error, Parser, Data) when is_binary(Data) ->
    Output = iolist_to_binary(Data),
    Size = min(byte_size(Output), 20),
    LimitedData = <<Output:Size/binary, "...">>,
    throw_error(Error, Parser, ?E_PARSE, LimitedData);
throw_error(Error, Parser, Data) when is_tuple(Data) ->
    throw_error(Error, Parser, ?E_PARSE, Data).

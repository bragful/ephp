-module(ephp_parser).
-compile([export_all]).

-include("ephp.hrl").

-define(IS_SPACE(X),
    erlang:'or'(X =:= <<" ">>,
                erlang:'or'(X =:= <<"\t">>,
                            X =:= <<"\r">>)
               )
).
-define(IS_NEWLINE(X),
    X =:= <<"\n">>
).
-define(IS_NUMBER(X),
    erlang:'and'(X >= $0, X =< $9)
).
-define(IS_ALPHA(X),
    erlang:'or'(
        erlang:'and'(X >= $A, X =< $Z),
        erlang:'and'(X >= $a, X =< $z)
    )
).
-define(OR(I,X,Y), erlang:'or'(I =:= X,I =:= Y)).
-define(IS_OP1(X),
    X =:= <<126>> orelse
    X =:= <<"@">> orelse
    X =:= <<"!">> orelse
    X =:= <<"*">> orelse
    X =:= <<"/">> orelse
    X =:= <<"%">> orelse
    X =:= <<"+">> orelse
    X =:= <<"-">> orelse
    X =:= <<".">> orelse
    X =:= <<"<">> orelse
    X =:= <<">">> orelse
    X =:= <<"&">> orelse
    X =:= <<"^">> orelse
    X =:= <<"|">>
).
-define(IS_OP2(X),
    X =:= <<"**">> orelse
    X =:= <<"++">> orelse
    X =:= <<"--">> orelse
    X =:= <<"<<">> orelse
    X =:= <<">>">> orelse
    X =:= <<"<=">> orelse
    X =:= <<">=">> orelse
    X =:= <<"==">> orelse
    X =:= <<"!=">> orelse
    X =:= <<"<>">> orelse
    X =:= <<"&&">> orelse
    X =:= <<"||">> orelse
    X =:= <<"??">> orelse
    X =:= <<"+=">> orelse
    X =:= <<"-=">> orelse
    X =:= <<"*=">> orelse
    X =:= <<"/=">> orelse
    X =:= <<".=">> orelse
    X =:= <<"%=">> orelse
    X =:= <<"&=">> orelse
    X =:= <<"|=">> orelse
    X =:= <<"^=">> orelse
    X =:= <<"or">> orelse X =:= <<"OR">> orelse
    X =:= <<"Or">> orelse X =:= <<"oR">>
).
-define(IS_OP3(X),
    X =:= <<"===">> orelse
    X =:= <<"!==">> orelse
    X =:= <<"<=>">> orelse
    X =:= <<"**=">> orelse
    X =:= <<"<<=">> orelse
    X =:= <<">>=">> orelse
    X =:= <<"and">> orelse X =:= <<"And">> orelse X =:= <<"aNd">> orelse
    X =:= <<"ANd">> orelse X =:= <<"anD">> orelse X =:= <<"AnD">> orelse
    X =:= <<"aND">> orelse X =:= <<"AND">> orelse
    X =:= <<"xor">> orelse X =:= <<"Xor">> orelse X =:= <<"xOr">> orelse
    X =:= <<"XOr">> orelse X =:= <<"xoR">> orelse X =:= <<"XoR">> orelse
    X =:= <<"xOR">> orelse X =:= <<"XOR">>
).

parse(Document) ->
    {_, _, Parsed} = document(Document, {code,1,1}, []),
    lists:reverse(Parsed).

document(<<>>, Pos, Parsed) ->
    {<<>>, Pos, Parsed};
document(<<"<?php",Rest/binary>>, Pos, Parsed) ->
    code(Rest, add_pos(Pos,5), Parsed);
document(<<"<?=",Rest/binary>>, Pos, Parsed) ->
    code_value(Rest, code_value_level(add_pos(Pos,3)), Parsed);
document(<<"<?",Rest/binary>>, Pos, Parsed) ->
    %% TODO: if short is not permitted, use as text
    code(Rest, add_pos(Pos,2), Parsed);
document(<<"\n",Rest/binary>>, Pos, Parsed) ->
    document(Rest, new_line(Pos), add_to_text(<<"\n">>, Pos, Parsed));
document(<<L:1/binary,Rest/binary>>, Pos, Parsed) ->
    document(Rest, add_pos(Pos,1), add_to_text(L, Pos, Parsed)).

code(<<>>, Pos, Parsed) ->
    {<<>>, Pos, Parsed};
code(<<"}",Rest/binary>>, {code_block,_,_}=Pos, Parsed) ->
    {Rest, normal_level(add_pos(Pos,1)), Parsed};
code(<<";",Rest/binary>>, {code_statement,_,_}=Pos, Parsed) ->
    {Rest, normal_level(add_pos(Pos,1)), Parsed};
code(<<I:8,F:8,SP:8,Rest/binary>>, Pos, Parsed)
        when ?OR(I,$i,$I) andalso ?OR(F,$f,$F) andalso ?OR(SP,32,$() ->
    st_if(<<SP:8,Rest/binary>>, Pos, Parsed);
code(<<E:8,L:8,S:8,E:8,SP:8,Rest/binary>>,
     Pos, [#if_block{}|_]=Parsed)
        when ?OR(E,$e,$E) andalso ?OR(L,$l,$L) andalso ?OR(S,$s,$S)
        andalso (?OR(SP,32,${) orelse ?OR(SP,$i,$I)) ->
    st_else(<<SP:8,Rest/binary>>, Pos, Parsed);
code(<<"?>",Rest/binary>>, {code_value,_,_}=Pos, Parsed) ->
    {Rest, normal_level(add_pos(Pos,2)), Parsed};
code(<<"?>",Rest/binary>>, Pos, Parsed) ->
    document(Rest, add_pos(Pos,2), Parsed);
code(<<"//",Rest/binary>>, Pos, Parsed) ->
    comment_line(Rest, Pos, Parsed);
code(<<"/*",Rest/binary>>, Pos, Parsed) ->
    comment_block(Rest, Pos, Parsed);
code(<<Space:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(Space) ->
    code(Rest, add_pos(Pos,1), Parsed);
code(<<NewLine:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(NewLine) ->
    code(Rest, new_line(Pos), Parsed);
code(<<_/utf8,_>> = Text, Pos, _) ->
    throw({error, {parse, Pos, sample_text(Text)}}).

code_value(Text, {_Level,Row,Col}=Pos, Parsed) ->
    {Rest, NewPos, Code} = code(Text, code_value_level(Pos), []),
    document(Rest, NewPos, [#print{expression=Code,line={Row,Col}}|Parsed]).

code_block(<<"{",Rest/binary>>, Pos, Parsed) ->
    code(Rest, code_block_level(add_pos(Pos,1)), Parsed);
code_block(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    code_block(Rest, add_pos(Pos,1), Parsed);
code_block(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    code_block(Rest, new_line(Pos), Parsed);
code_block(<<>>, Pos, Parsed) ->
    {<<>>, Pos, Parsed};
code_block(Rest, Pos, Parsed) ->
    code(Rest, code_statement_level(Pos), Parsed).

conditions(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    conditions(Rest, add_pos(Pos,1), Parsed);
conditions(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    conditions(Rest, new_line(Pos), Parsed);
conditions(<<"(",Rest/binary>>, {0,Row,Col}, Parsed) ->
    conditions(Rest, {1,Row,Col+1}, Parsed);
conditions(<<"(",Rest/binary>>, {Level,Row,Col}, Parsed) ->
    conditions(Rest, {Level+1,Row,Col+1}, add_op(open, Parsed));
conditions(<<")",Rest/binary>>, {1,_Row,_Col}=Pos, Parsed) ->
    {Rest, normal_level(add_pos(Pos,1)), add_op('end', Parsed)};
conditions(<<")",Rest/binary>>, {Level,Row,Col}, Parsed) ->
    conditions(Rest, {Level-1,Row,Col+1}, add_op(close, Parsed));
conditions(<<"$",Rest/binary>>, Pos, Parsed) ->
    {Rest0, Pos0, [Var]} = variable(Rest, add_pos(Pos,1), []),
    conditions(Rest0, Pos0, add_op(Var, Parsed));
conditions(<<Op:3/binary,Rest/binary>>, Pos, Parsed) when ?IS_OP3(Op) ->
    conditions(Rest, add_pos(Pos,3), add_op({Op,precedence(Op)}, Parsed));
conditions(<<Op:2/binary,Rest/binary>>, Pos, Parsed) when ?IS_OP2(Op) ->
    conditions(Rest, add_pos(Pos,2), add_op({Op,precedence(Op)}, Parsed));
conditions(<<Op:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_OP1(Op) ->
    conditions(Rest, add_pos(Pos,1), add_op({Op,precedence(Op)}, Parsed));
conditions(<<A:8,_/binary>> = Rest, Pos, Parsed) when ?IS_NUMBER(A) ->
    {Rest0, Pos0, [Number]} = number(Rest, Pos, []),
    conditions(Rest0, Pos0, add_op(Number, Parsed));
conditions(<<A:8,_/binary>> = Rest, Pos, Parsed) when ?IS_ALPHA(A) ->
    {Rest0, Pos0, Constant} = constant(Rest, Pos, []),
    conditions(Rest0, Pos0, add_op(Constant, Parsed));
conditions(<<A:8,_/binary>> = Rest, Pos, Parsed) when A =:= $" orelse A =:= $' ->
    {Rest0, Pos0, [String]} = string(Rest, Pos, []),
    conditions(Rest0, Pos0, add_op(String, Parsed));
conditions(<<>>, Pos, _Parsed) ->
    throw({error, {parse, Pos, incomplete_conditions}}).

variable(<<A:8,Rest/binary>>, {_,Row,Col}=Pos, []) when ?IS_ALPHA(A) ->
    Var = #variable{name = <<A:8>>, line={Row,Col}},
    variable(Rest, add_pos(Pos,1), [Var]);
variable(<<A:8,Rest/binary>>, {_,_,_}=Pos, [#variable{name=N}=V])
        when ?IS_NUMBER(A) orelse ?IS_ALPHA(A) orelse A =:= $_ ->
    variable(Rest, add_pos(Pos,1), [V#variable{name = <<N/binary,A:8>>}]);
variable(<<SP:1/binary,Rest/binary>>, {enclosed,_,_}=Pos, Var) when ?IS_SPACE(SP) ->
    variable(Rest, add_pos(Pos,1), Var);
variable(<<SP:1/binary,Rest/binary>>, {enclosed,_,_}=Pos, Var) when ?IS_NEWLINE(SP) ->
    variable(Rest, add_pos(Pos,1), Var);
variable(<<"}",Rest/binary>>, {enclosed,_,_}=Pos, Var) ->
    {Rest, add_pos(Pos,1), Var};
variable(Rest, Pos, Parsed) ->
    {Rest, Pos, Parsed}.

number(<<A:8,Rest/binary>>, {_,Row,Col}=Pos, []) when ?IS_NUMBER(A) ->
    number(Rest, add_pos(Pos,1), [#int{int = <<A:8>>, line={Row,Col}}]);
number(<<A:8,Rest/binary>>, Pos, [#int{int=N}=I]) when ?IS_NUMBER(A) ->
    number(Rest, add_pos(Pos,1), [I#int{int = <<N/binary,A:8>>}]);
number(<<".",Rest/binary>>, Pos, [#int{int=N,line=Line}]) ->
    number(Rest, add_pos(Pos,1), [#float{float = <<N/binary,".">>,line=Line}]);
number(<<A:8,Rest/binary>>, Pos, [#float{float=N}=F]) when ?IS_NUMBER(A) ->
    number(Rest, add_pos(Pos,1), [F#float{float = <<N/binary,A:8>>}]);
number(Rest, Pos, [#int{int=N}=I]) ->
    {Rest, Pos, [I#int{int=binary_to_integer(N)}]};
number(Rest, Pos, [#float{float=N}=F]) ->
    {Rest, Pos, [F#float{float=binary_to_float(N)}]}.

constant(<<A:8,Rest/binary>>, {_,Row,Col}=Pos, []) when ?IS_ALPHA(A) ->
    constant(Rest, add_pos(Pos,1), [#constant{name = <<A:8>>, line={Row,Col}}]);
constant(<<A:8,Rest/binary>>, Pos, [#constant{name=N}=C])
        when ?IS_ALPHA(A) orelse ?IS_NUMBER(A) orelse A =:= $_ ->
    constant(Rest, add_pos(Pos,1), [C#constant{name = <<N/binary, A:8>>}]);
constant(Rest, Pos, Parsed) ->
    {Rest, Pos, Parsed}.

string(<<"\"",Rest/binary>>, {_,Row,Col}=Pos, []) ->
    string_parsed(Rest, Pos, [#text_to_process{text = [], line={Row,Col}}]);
string(<<"'",Rest/binary>>, {_,Row,Col}=Pos, []) ->
    string_fixed(Rest, Pos, [#text{text = <<>>, line={Row,Col}}]).

string_fixed(<<"\\\\",Rest/binary>>, Pos, [#text{text=C}=S]) ->
    string_fixed(Rest, add_pos(Pos,1), [S#text{text = <<C/binary, "\\\\">>}]);
string_fixed(<<"\\'",Rest/binary>>, Pos, [#text{text=C}=S]) ->
    string_fixed(Rest, add_pos(Pos,1), [S#text{text = <<C/binary, "\\'">>}]);
string_fixed(<<"'",Rest/binary>>, Pos, Parsed) ->
    {Rest, add_pos(Pos,1), Parsed};
string_fixed(<<"\n",Rest/binary>>, Pos, [#text{text=C}=S]) ->
    string_fixed(Rest, new_line(Pos), [S#text{text = <<C/binary, "\n">>}]);
string_fixed(<<A/utf8,Rest/binary>>, Pos, [#text{text=C}=S]) ->
    string_fixed(Rest, add_pos(Pos,1), [S#text{text = <<C/binary, A/utf8>>}]).

string_parsed(<<"\\\\",Rest/binary>>, Pos, [#text_to_process{text=[C|R]}=S])
        when is_binary(C) ->
    string_parsed(Rest, add_pos(Pos,1), [S#text_to_process{text = [<<C/binary, "\\\\">>|R]}]);
string_parsed(<<"\\\"",Rest/binary>>, Pos, [#text_to_process{text=[C|R]}=S])
        when is_binary(C)  ->
    string_parsed(Rest, add_pos(Pos,1), [S#text_to_process{text = [<<C/binary, "\\\"">>|R]}]);
string_parsed(<<"\"",Rest/binary>>, Pos, [#text_to_process{text=C}=S|Parsed]) ->
    {Rest, add_pos(Pos,1), [S#text_to_process{text=lists:reverse(C)}|Parsed]};
string_parsed(<<"\n",Rest/binary>>, Pos, [#text_to_process{text=[C|R]}=S|Parsed])
        when is_binary(C) ->
    string_parsed(Rest, new_line(Pos), [S#text_to_process{text = [<<C/binary, "\n">>|R]}|Parsed]);
string_parsed(<<"${",Rest/binary>>, {Level,Row,Col}, [#text_to_process{text=C}=S|Parsed]) ->
    {Rest0, {_,Row0,Col0}, [Var]} = variable(Rest, {enclosed,Row,Col+2}, []),
    string_parsed(Rest0, {Level,Row0,Col0}, [S#text_to_process{text=[Var|C]}|Parsed]);
string_parsed(<<"{$",Rest/binary>>, {Level,Row,Col}, [#text_to_process{text=C}=S|Parsed]) ->
    {Rest0, {_,Row0,Col0}, [Var]} = variable(Rest, {enclosed,Row,Col+2}, []),
    string_parsed(Rest0, {Level,Row0,Col0}, [S#text_to_process{text=[Var|C]}|Parsed]);
string_parsed(<<"$",Rest/binary>>, {Level,Row,Col}, [#text_to_process{text=C}=S|Parsed]) ->
    {Rest0, {_,Row0,Col0}, [Var]} = variable(Rest, {Level,Row,Col+1}, []),
    string_parsed(Rest0, {Level,Row0,Col0}, [S#text_to_process{text=[Var|C]}|Parsed]);
string_parsed(<<A/utf8,Rest/binary>>, Pos, [#text_to_process{text=[C|R]}=S|Parsed])
        when is_binary(C) ->
    string_parsed(Rest, add_pos(Pos,1), [S#text_to_process{text = [<<C/binary, A/utf8>>|R]}|Parsed]);
string_parsed(Rest, Pos, [#text_to_process{text=C}=S|Parsed]) ->
    string_parsed(Rest, Pos, [S#text_to_process{text=[<<>>|C]}|Parsed]).


st_if(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    st_if(Rest, add_pos(Pos,1), Parsed);
st_if(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    st_if(Rest, new_line(Pos), Parsed);
st_if(<<"(",_/binary>> = Rest0, {Level,Row,Col}, Parsed) ->
    {Rest1, Pos1, Conditions} = conditions(Rest0, add_pos({0,Row,Col},2), []),
    {Rest2, {_,Row2,Col2}, CodeBlock} = code_block(Rest1, Pos1, []),
    If = #if_block{
        conditions=Conditions,
        true_block=CodeBlock,
        line={Row,Col}
    },
    code(Rest2, {Level,Row2,Col2}, [If|Parsed]);
st_if(<<>>, Pos, _Parsed) ->
    throw({error, {parse, Pos, incomplete_if_statement}}).

st_else(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    st_else(Rest, add_pos(Pos,1), Parsed);
st_else(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    st_else(Rest, new_line(Pos), Parsed);
st_else(Rest0, {Level,_,_}=Pos0, [#if_block{}=If|Parsed]) ->
    {Rest1, {_,Row1,Col1}, CodeBlock} = code_block(Rest0, add_pos(Pos0,4), []),
    IfWithElse = If#if_block{false_block=CodeBlock},
    code(Rest1, {Level,Row1,Col1}, [IfWithElse|Parsed]);
st_else(<<>>, Pos, _Parsed) ->
    throw({error, {parse, Pos, incomplete_else_statement}}).

comment_line(<<>>, _Pos, Parsed) ->
    Parsed;
comment_line(<<"\n",Rest/binary>>, Pos, Parsed) ->
    code(Rest, new_line(Pos), Parsed);
comment_line(<<_/utf8,Rest/binary>>, Pos, Parsed) ->
    comment_line(Rest, add_pos(Pos,1), Parsed).

comment_block(<<>>, Pos, _Parsed) ->
    %% TODO: throw parse error
    throw({error, {parse, Pos, missng_comment_end}});
comment_block(<<"\n",Rest/binary>>, Pos, Parsed) ->
    comment_block(Rest, new_line(Pos), Parsed);
comment_block(<<"*/",Rest/binary>>, Pos, Parsed) ->
    code(Rest, add_pos(Pos,2), Parsed);
comment_block(<<_/utf8,Rest/binary>>, Pos, Parsed) ->
    comment_block(Rest, add_pos(Pos,1), Parsed).

%%------------------------------------------------------------------------------
%% helper functions
%%------------------------------------------------------------------------------

add_to_text(L, _Pos, [#text{text=Text}=T|Parsed]) ->
    NewText = <<Text/binary, L/binary>>,
    [T#text{text=NewText}|Parsed];
add_to_text(L, {_Level,Row,Col}, Parsed) ->
    [#text{text=L, line={Row,Col}}|Parsed].

add_pos({Level,Row,Col}, Offset) ->
    {Level,Row,Col+Offset}.

new_line({Level,Row,_Col}) ->
    {Level,Row+1,1}.

sample_text(<<Text:10/binary,_/binary>>) ->
    Text;
sample_text(Text) ->
    Text.

normal_level({_,Row,Col}) -> {code,Row,Col}.
code_block_level({_,Row,Col}) -> {code_block,Row,Col}.
code_value_level({_,Row,Col}) -> {code_value,Row,Col}.
code_statement_level({_,Row,Col}) -> {code_statement,Row,Col}.

% TODO: change this implementation for calculate operations
add_op('end', [{op,Content}|Parsed]) ->
    [{op,lists:reverse(Content)}|Parsed];
add_op(Add, [{op,Content}|Parsed]) ->
    [{op,[Add|Content]}|Parsed];
add_op(Add, Parsed) ->
    [{op,[Add]}|Parsed].

%% took from http://php.net/manual/en/language.operators.precedence.php

-type associativity() :: no_assoc | left | right.
-spec precedence(binary()) -> {associativity(), pos_integer()} | false.

precedence(<<"clone">>) -> {no_assoc, 1};
precedence(<<"new">>) -> {no_assoc, 1};
precedence(<<"[">>) -> {left, 2}; %% array
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
precedence(<<"@">>) -> {right, 4};
precedence(<<"instaceof">>) -> {no_assoc, 5};
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
precedence(<<"?:">>) -> {left, 18}; %% ternary
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

-module(ephp_parser).
-compile([export_all]).

-record(text, {content, row, col}).
-record(print, {text, row, col}).
-record(st_if, {conditions, then, else, row, col}).
-record(operation, {content, row, col}).
-record(variable, {name, row, col}).
-record(number, {number, type, row, col}).
-record(constant, {name, row, col}).
-record(string, {content, row, col}).

-define(IS_SPACE(X),
    X =:= <<" ">> orelse
    X =:= <<"\t">> orelse
    X =:= <<"\r">>
).
-define(IS_NEWLINE(X),
    X =:= <<"\n">>
).
-define(IS_NUMBER(X),
    X >= $0 andalso X =< $9
).
-define(IS_ALPHA(X),
    ((X >= $A andalso X =< $Z) orelse (X >= $a andalso X =< $z))
).
-define(IS_OP1(X),
    X =:= <<"+">> orelse
    X =:= <<"-">> orelse
    X =:= <<"*">> orelse
    X =:= <<"/">> orelse
    X =:= <<">">> orelse
    X =:= <<"<">> orelse
    X =:= <<"%">>
).
-define(IS_OP2(X),
    X =:= <<"==">> orelse
    X =:= <<"<=">> orelse
    X =:= <<">=">> orelse
    X =:= <<"!=">>
).
-define(IS_OP3(X),
    X =:= <<"===">> orelse
    X =:= <<"!==">>
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
        when (I =:= $i orelse I =:= $I)
        andalso (F =:= $f orelse F =:= $F)
        andalso (SP =:= 32 orelse SP =:= $() ->
    st_if(<<SP:8,Rest/binary>>, Pos, Parsed);
code(<<E:8,L:8,S:8,E:8,SP:8,Rest/binary>>,
     Pos, [#st_if{}|_]=Parsed)
        when (E =:= $e orelse E =:= $E)
        andalso (L =:= $l orelse L =:= $L)
        andalso (S =:= $s orelse S =:= $S)
        andalso (SP =:= 32 orelse SP =:= ${ orelse SP =:= $i orelse SP =:= $I) ->
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
    document(Rest, NewPos, [#print{text=Code,row=Row,col=Col}|Parsed]).

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
conditions(<<"(",Rest/binary>>, {Level,Row,Col}, Parsed) ->
    conditions(Rest, {Level+1,Row,Col+1}, add_op(open, Parsed));
conditions(<<")",Rest/binary>>, {1,_Row,_Col}=Pos, Parsed) ->
    {Rest, normal_level(add_pos(Pos,1)), add_op(close, Parsed)};
conditions(<<")",Rest/binary>>, {Level,Row,Col}, Parsed) ->
    conditions(Rest, {Level-1,Row,Col+1}, add_op(close, Parsed));
conditions(<<"$",Rest/binary>>, Pos, Parsed) ->
    {Rest0, Pos0, Var} = variable(Rest, varname_level(add_pos(Pos,1)), []),
    conditions(Rest0, Pos0, add_op(Var, Parsed));
conditions(<<Op:3/binary,Rest/binary>>, Pos, Parsed) when ?IS_OP3(Op) ->
    conditions(Rest, add_pos(Pos,3), add_op(Op, Parsed));
conditions(<<Op:2/binary,Rest/binary>>, Pos, Parsed) when ?IS_OP2(Op) ->
    conditions(Rest, add_pos(Pos,2), add_op(Op, Parsed));
conditions(<<Op:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_OP1(Op) ->
    conditions(Rest, add_pos(Pos,1), add_op(Op, Parsed));
conditions(<<A:8,_/binary>> = Rest, Pos, Parsed) when ?IS_NUMBER(A) ->
    {Rest0, Pos0, [Number]} = number(Rest, Pos, []),
    conditions(Rest0, Pos0, add_op(Number, Parsed));
conditions(<<A:8,_/binary>> = Rest, Pos, Parsed) when ?IS_ALPHA(A) ->
    {Rest0, Pos0, Constant} = constant(Rest, Pos, []),
    conditions(Rest0, Pos0, add_op(Constant, Parsed));
conditions(<<A:8,_/binary>> = Rest, Pos, Parsed) when A =:= $" orelse A =:= $' ->
    {Rest0, Pos0, String} = string(Rest, Pos, []),
    conditions(Rest0, Pos0, add_op(String, Parsed));
conditions(<<>>, Pos, _Parsed) ->
    throw({error, {parse, Pos, incomplete_conditions}}).

variable(<<A:8,Rest/binary>>, {name,Row,Col}=Pos, []) when ?IS_ALPHA(A) ->
    Var = #variable{name = <<A:8>>, row=Row, col=Col},
    variable(Rest, add_pos(Pos,1), [Var]);
variable(<<A:8,Rest/binary>>, {name,_,_}=Pos, [#variable{name=N}=V])
        when ?IS_NUMBER(A) orelse ?IS_ALPHA(A) orelse A =:= $_ ->
    variable(Rest, add_pos(Pos,1), [V#variable{name = <<N/binary,A:8>>}]);
%% TODO: array indexes, objects, etc.
variable(Rest, Pos, Parsed) ->
    {Rest, Pos, Parsed}.

number(<<A:8,Rest/binary>>, {_,Row,Col}=Pos, []) when ?IS_NUMBER(A) ->
    number(Rest, add_pos(Pos,1), [#number{type=int,number = <<A:8>>,row=Row,col=Col}]);
number(<<A:8,Rest/binary>>, Pos, [#number{type=int,number=N}=D]) when ?IS_NUMBER(A) ->
    number(Rest, add_pos(Pos,1), [D#number{number = <<N/binary,A:8>>}]);
number(<<".",Rest/binary>>, Pos, [#number{type=int,number=N}=D]) ->
    number(Rest, add_pos(Pos,1), [D#number{type=float,number = <<N/binary,".">>}]);
number(<<A:8,Rest/binary>>, Pos, [#number{type=float,number=N}=D]) when ?IS_NUMBER(A) ->
    number(Rest, add_pos(Pos,1), [D#number{number = <<N/binary,A:8>>}]);
number(Rest, Pos, [#number{type=int,number=N}=D]) ->
    {Rest, Pos, [D#number{number=binary_to_integer(N)}]};
number(Rest, Pos, [#number{type=float,number=N}=D]) ->
    {Rest, Pos, [D#number{number=binary_to_float(N)}]}.

constant(<<A:8,Rest/binary>>, {_,Row,Col}=Pos, []) when ?IS_ALPHA(A) ->
    constant(Rest, add_pos(Pos,1), [#constant{name = <<A:8>>, row=Row, col=Col}]);
constant(<<A:8,Rest/binary>>, Pos, [#constant{name=N}=C])
        when ?IS_ALPHA(A) orelse ?IS_NUMBER(A) orelse A =:= $_ ->
    constant(Rest, add_pos(Pos,1), [C#constant{name = <<N/binary, A:8>>}]);
constant(Rest, Pos, Parsed) ->
    {Rest, Pos, Parsed}.

string(<<"\"",Rest/binary>>, {_,Row,Col}=Pos, []) ->
    string_parsed(Rest, Pos, [#string{content = <<>>, row=Row, col=Col}]);
string(<<"'",Rest/binary>>, {_,Row,Col}=Pos, []) ->
    string_fixed(Rest, Pos, [#string{content = <<>>, row=Row, col=Col}]).

string_fixed(<<"\\\\",Rest/binary>>, Pos, [#string{content=C}=S]) ->
    string_fixed(Rest, add_pos(Pos,1), [S#string{content = <<C/binary, "\\\\">>}]);
string_fixed(<<"\\'",Rest/binary>>, Pos, [#string{content=C}=S]) ->
    string_fixed(Rest, add_pos(Pos,1), [S#string{content = <<C/binary, "\\'">>}]);
string_fixed(<<"'",Rest/binary>>, Pos, Parsed) ->
    {Rest, add_pos(Pos,1), Parsed};
string_fixed(<<"\n",Rest/binary>>, Pos, [#string{content=C}=S]) ->
    string_fixed(Rest, new_line(Pos), [S#string{content = <<C/binary, "\n">>}]);
string_fixed(<<A/utf8,Rest/binary>>, Pos, [#string{content=C}=S]) ->
    string_fixed(Rest, add_pos(Pos,1), [S#string{content = <<C/binary, A/utf8>>}]).

string_parsed(<<"\\\\",Rest/binary>>, Pos, [#string{content=C}=S]) ->
    string_parsed(Rest, add_pos(Pos,1), [S#string{content = <<C/binary, "\\\\">>}]);
string_parsed(<<"\\\"",Rest/binary>>, Pos, [#string{content=C}=S]) ->
    string_parsed(Rest, add_pos(Pos,1), [S#string{content = <<C/binary, "\\\"">>}]);
string_parsed(<<"\"",Rest/binary>>, Pos, Parsed) ->
    {Rest, add_pos(Pos,1), Parsed};
string_parsed(<<"\n",Rest/binary>>, Pos, [#string{content=C}=S]) ->
    string_parsed(Rest, new_line(Pos), [S#string{content = <<C/binary, "\n">>}]);
%% TODO add the variable parsing
string_parsed(<<A/utf8,Rest/binary>>, Pos, [#string{content=C}=S]) ->
    string_parsed(Rest, add_pos(Pos,1), [S#string{content = <<C/binary, A/utf8>>}]).


st_if(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    st_if(Rest, add_pos(Pos,1), Parsed);
st_if(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    st_if(Rest, new_line(Pos), Parsed);
st_if(<<"(",_/binary>> = Rest0, {_Level,Row,Col}, Parsed) ->
    {Rest1, Pos1, Conditions} = conditions(Rest0, {0,Row,Col}, []),
    {Rest2, Pos2, CodeBlock} = code_block(Rest1, Pos1, []),
    If = #st_if{
        conditions=Conditions,
        then=CodeBlock,
        row=Row,
        col=Col
    },
    code(Rest2, Pos2, [If|Parsed]);
st_if(<<>>, Pos, _Parsed) ->
    throw({error, {parse, Pos, incomplete_if_statement}}).

st_else(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    st_else(Rest, add_pos(Pos,1), Parsed);
st_else(<<SP:1/binary,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    st_else(Rest, new_line(Pos), Parsed);
st_else(Rest0, Pos0, [#st_if{}=If|Parsed]) ->
    {Rest1, Pos1, CodeBlock} = code_block(Rest0, Pos0, []),
    IfWithElse = If#st_if{else=CodeBlock},
    code(Rest1, Pos1, [IfWithElse|Parsed]);
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

add_to_text(L, _Pos, [#text{content=Text}=T|Parsed]) ->
    NewText = <<Text/binary, L/binary>>,
    [T#text{content=NewText}|Parsed];
add_to_text(L, {_Level,Row,Col}, Parsed) ->
    [#text{content=L, row=Row, col=Col}|Parsed].

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
varname_level({_,Row,Col}) -> {name,Row,Col}.

% TODO: change this implementation for calculate operations
add_op(open, Parsed) -> add_op(<<"(">>, Parsed);
add_op(close, Parsed) -> add_op(<<")">>, Parsed);
add_op(#number{type=int, number=N}, Parsed) -> add_op(integer_to_binary(N), Parsed);
add_op(#number{type=float, number=N}, Parsed) -> add_op(float_to_binary(N), Parsed);
add_op(#constant{name=Name}, Parsed) -> add_op(Name, Parsed);
add_op(Add, [#operation{content=Content}=Op|Parsed]) ->
    [Op#operation{content = <<Content/binary, Add/binary>>}|Parsed];
add_op(Add, Parsed) ->
    [#operation{content = Add}|Parsed].

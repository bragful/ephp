-module(ephp_parser_func).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-export([function/3, call_args/3, st_function/3, st_use_or_block/3, echo/3,
         funct_args/3, funct_name/3]).

-import(ephp_parser, [
    add_line/2, add_pos/2, new_line/1, arg_level/1, copy_level/2,
    remove_spaces/2,

    variable/3, code/3, code_block/3,

    code_block_level/1
]).

echo(<<SP:8,Rest/binary>>, Pos, [#call{}|_]=Parsed) when ?IS_SPACE(SP) ->
    echo(Rest, add_pos(Pos,1), Parsed);
echo(<<SP:8,Rest/binary>>, Pos, [#call{}|_]=Parsed) when ?IS_NEWLINE(SP) ->
    echo(Rest, new_line(Pos), Parsed);
echo(<<";",Rest/binary>>, Pos, Parsed) ->
    {Rest, add_pos(Pos,1), Parsed};
echo(Rest, Pos, [#call{args=Args}=C|Parsed]) when Rest =/= <<>> ->
    case ephp_parser_expr:expression(Rest, arg_level(Pos), []) of
        {<<";",_/binary>> = Rest0, Pos0, []} ->
            {Rest0, Pos0, [C|Parsed]};
        {<<";",_/binary>> = Rest0, Pos0, Arg} ->
            {Rest0, Pos0, [C#call{args=Args ++ [Arg]}|Parsed]};
        {<<"?>",_/binary>> = Rest0, Pos0, Arg} ->
            {Rest0, Pos0, [C#call{args=Args ++ [Arg]}|Parsed]};
        {<<",",Rest0/binary>>, Pos0, Arg} ->
            NewCall = C#call{args=Args ++ [Arg]},
            echo(Rest0, add_pos(Pos0, 1), [NewCall|Parsed]);
        {Rest0, Pos0, []} ->
            echo(Rest0, Pos0, [C|Parsed]);
        {Rest0, Pos0, Arg} ->
            echo(Rest0, Pos0, [C#call{args=Args ++ [Arg]}|Parsed])
    end.

function(Rest, Pos, [#call{} = Call|Parsed]) ->
    {Rest0, Pos0, Args} = call_args(Rest, Pos, []),
    NewCall = Call#call{args = Args},
    {Rest0, Pos0, [NewCall|Parsed]}.

call_args(<<SP:8,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    call_args(Rest, add_pos(Pos, 1), Parsed);
call_args(<<SP:8,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    call_args(Rest, new_line(Pos), Parsed);
call_args(<<")",Rest/binary>>, Pos, Parsed) ->
    {Rest, add_pos(Pos, 1), Parsed};
call_args(Rest, Pos, Parsed) when Rest =/= <<>> ->
    case ephp_parser_expr:expression(Rest, arg_level(Pos), []) of
        {<<")",Rest0/binary>>, Pos0, []} ->
            {Rest0, add_pos(Pos0,1), Parsed};
        {<<")",Rest0/binary>>, Pos0, Arg} ->
            {Rest0, add_pos(Pos0,1), Parsed ++ [Arg]};
        {<<",",Rest0/binary>>, Pos0, Arg} ->
            call_args(Rest0, add_pos(Pos0, 1), Parsed ++ [Arg]);
        {<<";",_/binary>>, Pos0, _} ->
            ephp_parser:throw_error(eparse, Pos0, {<<";">>});
        {Rest, Pos, _} ->
            ephp_parser:throw_error(eparse, Pos, Rest);
        {Rest0, Pos0, []} ->
            call_args(Rest0, Pos0, Parsed);
        {Rest0, Pos0, Arg} ->
            call_args(Rest0, Pos0, Parsed ++ [Arg])
    end.

st_use(<<SP:8,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    st_use(Rest, add_pos(Pos,1), Parsed);
st_use(<<SP:8,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    st_use(Rest, new_line(Pos), Parsed);
st_use(<<",",Rest/binary>>, Pos, Parsed) ->
    st_use(Rest, add_pos(Pos,1), Parsed);
st_use(<<"(",Rest/binary>>, Pos, Parsed) ->
    st_use(Rest, add_pos(Pos,1), Parsed);
st_use(<<")",Rest/binary>>, Pos, Parsed) ->
    {Rest, add_pos(Pos,1), Parsed};
st_use(<<"&$",Rest/binary>>, Pos, Parsed) ->
    {Rest0, Pos0, [Var]} = variable(<<"$",Rest/binary>>, add_pos(Pos,1), []),
    st_use(Rest0, Pos0, [add_line(#ref{var=Var}, Pos)|Parsed]);
st_use(<<"$",_/binary>> = Rest, Pos, Parsed) ->
    {Rest0, Pos0, [Var]} = variable(Rest, Pos, []),
    st_use(Rest0, Pos0, [add_line(Var, Pos)|Parsed]).

st_use_or_block(<<SP:8,Rest/binary>>, Pos, Function) when ?IS_SPACE(SP) ->
    st_use_or_block(Rest, add_pos(Pos,1), Function);
st_use_or_block(<<SP:8,Rest/binary>>, Pos, Function) when ?IS_NEWLINE(SP) ->
    st_use_or_block(Rest, new_line(Pos), Function);
st_use_or_block(<<"{",Rest/binary>>, Pos, Function) ->
    {Rest0, Pos0, CodeBlock} =
        code(Rest, code_block_level(add_pos(Pos,1)), []),
    {Rest0, Pos0, Function#function{code=CodeBlock}};
st_use_or_block(<<U:8,S:8,E:8,SP:8,Rest/binary>>, Pos, Function) when
        ?OR(U,$U,$u) andalso ?OR(S,$S,$s) andalso ?OR(E,$E,$e) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Pos0, Use} = st_use(Rest, Pos, []),
    st_use_or_block(Rest0, Pos0, Function#function{use=Use}).

st_function(<<SP:8,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    st_function(Rest, add_pos(Pos,1), Parsed);
st_function(<<SP:8,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    st_function(Rest, new_line(Pos), Parsed);
% TODO if the following char is '(' maybe this is a anon-function
st_function(Rest, Pos, Parsed) ->
    ReturnRef = case remove_spaces(Rest, Pos) of
        {<<"&",Rest0/binary>>, Pos0} -> true;
        {Rest0, Pos0} -> false
    end,
    {Rest1, Pos1, Name} = funct_name(Rest0, Pos0, []),
    {<<"(",Rest2/binary>>, Pos2} = remove_spaces(Rest1, Pos1),
    {Rest3, Pos3, Args} = funct_args(Rest2, Pos2, []),
    {Rest4, Pos4, CodeBlock} = code_block(Rest3, Pos3, []),
    Function = add_line(#function{
        name = Name,
        args = Args,
        code = CodeBlock,
        return_ref = ReturnRef
    }, Pos),
    {Rest4, copy_level(Pos, Pos4), [Function|Parsed]}.

funct_name(<<A:8,Rest/binary>>, Pos, []) when ?IS_ALPHA(A) orelse A =:= $_ ->
    funct_name(Rest, add_pos(Pos,1), [<<A:8>>]);
funct_name(<<A:8,Rest/binary>>, Pos, [N])
        when ?IS_ALPHA(A) orelse ?IS_NUMBER(A) orelse A =:= $_ ->
    funct_name(Rest, add_pos(Pos,1), [<<N/binary, A:8>>]);
funct_name(Rest, Pos, [N]) ->
    {Rest, Pos, N}.

funct_args(<<SP:8,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    funct_args(Rest, add_pos(Pos,1), Parsed);
funct_args(<<SP:8,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    funct_args(Rest, new_line(Pos), Parsed);
funct_args(<<"&",Rest/binary>>, Pos, Parsed) ->
    {Rest0, Pos0, [Var|Parsed0]} = funct_args(Rest, Pos, Parsed),
    {Rest0, Pos0, [add_line(#ref{var=Var}, Pos)|Parsed0]};
funct_args(<<",",Rest/binary>>, Pos, Parsed) ->
    funct_args(Rest, add_pos(Pos,1), Parsed);
funct_args(<<")",Rest/binary>>, Pos, Parsed) ->
    {Rest, add_pos(Pos,1), lists:reverse(Parsed)};
funct_args(<<A:8,_/binary>> = Rest, Pos, Parsed)
        when ?IS_ALPHA(A) orelse A =:= $_ ->
    {Rest0, Pos0, [#constant{name = Constant}]} =
        ephp_parser:constant(Rest, Pos, []),
    {Rest1, Pos1, [Var]} = variable(Rest0, Pos0, []),
    case remove_spaces(Rest1, Pos1) of
        {<<"=",Rest2/binary>>, Pos2} ->
            NewPos = arg_level(add_pos(Pos2, 1)),
            {Rest3, Pos3, Default} =
                ephp_parser_expr:expression(Rest2, NewPos, []),
            NewVar = Var#variable{default_value = Default,
                                  data_type = Constant},
            funct_args(Rest3, copy_level(Pos, Pos3), [NewVar|Parsed]);
        {Rest2, Pos2} ->
            NewVar = Var#variable{data_type = Constant},
            funct_args(Rest2, add_pos(Pos2, 1), [NewVar|Parsed])
    end;
funct_args(<<"$",_/binary>> = Rest, Pos, Parsed) ->
    {Rest0, Pos0, [Var]} = variable(Rest, Pos, []),
    case remove_spaces(Rest0, Pos0) of
        {<<"=",Rest1/binary>>, Pos1} ->
            NewPos = arg_level(add_pos(Pos1, 1)),
            {Rest2, Pos2, Default} =
                ephp_parser_expr:expression(Rest1, NewPos, []),
            NewVar = Var#variable{default_value = Default},
            funct_args(Rest2, copy_level(Pos, Pos2), [NewVar|Parsed]);
        {Rest1, Pos1} ->
            funct_args(Rest1, add_pos(Pos1, 1), [Var|Parsed])
    end.

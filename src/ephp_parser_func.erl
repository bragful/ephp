-module(ephp_parser_func).
-author('manuel@altenwald.com').
-compile([warnings_as_errors, export_all]).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-export([function/3, st_function/3, st_use_or_block/3]).

-import(ephp_parser, [
    add_line/2, add_pos/2, new_line/1, arg_level/1, copy_level/2,
    remove_spaces/2,

    variable/3, code/3, code_block/3,

    code_block_level/1
]).

function(<<SP:8,Rest/binary>>, Pos, [#call{}|_]=Parsed) when ?IS_SPACE(SP) ->
    function(Rest, add_pos(Pos,1), Parsed);
function(<<SP:8,Rest/binary>>, Pos, [#call{}|_]=Parsed) when ?IS_NEWLINE(SP) ->
    function(Rest, new_line(Pos), Parsed);
function(<<")",Rest/binary>>, Pos, Parsed) ->
    {Rest,add_pos(Pos,1),Parsed};
%% TODO error missing closing params
function(Rest, Pos, [#call{args=Args}=C|Parsed]) when Rest =/= <<>> ->
    case ephp_parser_expr:expression(Rest, arg_level(Pos), []) of
        {<<")",Rest0/binary>>, Pos0, []} ->
            {Rest0, add_pos(Pos0,1), [C|Parsed]};
        {<<")",Rest0/binary>>, Pos0, Arg} ->
            {Rest0, add_pos(Pos0,1), [C#call{args=Args ++ [Arg]}|Parsed]};
        {<<",",Rest0/binary>>, Pos0, Arg} ->
            NewCall = C#call{args=Args ++ [Arg]},
            function(Rest0, add_pos(Pos0, 1), [NewCall|Parsed]);
        %% TODO error missing closing params
        {Rest0, Pos0, []} ->
            function(Rest0, Pos0, [C|Parsed]);
        {Rest0, Pos0, Arg} ->
            function(Rest0, Pos0, [C#call{args=Args ++ [Arg]}|Parsed])
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
    {Rest0, Pos0, Name} = funct_name(Rest, Pos, []),
    {<<"(",Rest1/binary>>, Pos1} = remove_spaces(Rest0, Pos0),
    {Rest2, Pos2, Args} = funct_args(Rest1, Pos1, []),
    {Rest3, Pos3, CodeBlock} = code_block(Rest2, Pos2, []),
    Function = add_line(#function{
        name = Name,
        args = Args,
        code = CodeBlock
    }, Pos),
    {Rest3, copy_level(Pos, Pos3), [Function|Parsed]}.

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
funct_args(Rest, Pos, Parsed) ->
    {Rest0, Pos0, [Var]} = variable(Rest, Pos, []),
    case remove_spaces(Rest0, Pos0) of
        {<<"=",Rest1/binary>>, Pos1} ->
            NewPos = arg_level(add_pos(Pos1,1)),
            {Rest2, Pos2, Default} =
                ephp_parser_expr:expression(Rest1, NewPos, []),
            NewVar = add_line(Var#variable{default_value = Default}, Pos),
            funct_args(Rest2, copy_level(Pos, Pos2), [NewVar|Parsed]);
        {Rest1, Pos1} ->
            funct_args(Rest1, add_pos(Pos1,1), [Var|Parsed])
    end.

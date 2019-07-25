-module(ephp_parser_func).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-export([function/3, call_args/3, st_function/3, st_use_or_block/3, echo/3,
         funct_args/3, funct_name/3, get_ns/2]).

-import(ephp_parser, [
    add_line/2, add_pos/2, new_line/1, arg_level/1, copy_rowcol/2,
    remove_spaces/2, inc_pos/1,

    variable/3, code/3, code_block/3,

    code_block_level/1
]).

echo(<<SP:8,Rest/binary>>, Parser, [#call{}|_]=Parsed) when ?IS_SPACE(SP) ->
    echo(Rest, inc_pos(Parser), Parsed);
echo(<<SP:8,Rest/binary>>, Parser, [#call{}|_]=Parsed) when ?IS_NEWLINE(SP) ->
    echo(Rest, new_line(Parser), Parsed);
echo(<<";",Rest/binary>>, Parser, Parsed) ->
    {Rest, inc_pos(Parser), Parsed};
echo(Rest, Parser, [#call{args = Args} = C|Parsed]) when Rest =/= <<>> ->
    case ephp_parser_expr:expression(Rest, arg_level(Parser), []) of
        {<<";",_/binary>> = Rest0, Parser0, []} ->
            {Rest0, Parser0, [C|Parsed]};
        {<<";",_/binary>> = Rest0, Parser0, Arg} ->
            {Rest0, Parser0, [C#call{args = Args ++ [Arg]}|Parsed]};
        {<<"?>",_/binary>> = Rest0, Parser0, Arg} ->
            {Rest0, Parser0, [C#call{args = Args ++ [Arg]}|Parsed]};
        {<<",",Rest0/binary>>, Parser0, Arg} ->
            NewCall = C#call{args = Args ++ [Arg]},
            echo(Rest0, inc_pos(Parser0), [NewCall|Parsed]);
        {Rest0, Parser0, []} ->
            echo(Rest0, Parser0, [C|Parsed]);
        {Rest0, Parser0, Arg} ->
            echo(Rest0, Parser0, [C#call{args = Args ++ [Arg]}|Parsed])
    end.

function(Rest, Parser, [#call{} = Call|Parsed]) ->
    {Rest0, Parser0, Args} = call_args(Rest, Parser, []),
    NewCall = Call#call{args = Args},
    {Rest0, Parser0, [NewCall|Parsed]}.

call_args(<<SP:8, Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    call_args(Rest, inc_pos(Parser), Parsed);
call_args(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    call_args(Rest, new_line(Parser), Parsed);
call_args(<<")",Rest/binary>>, Parser, Parsed) ->
    {Rest, inc_pos(Parser), Parsed};
call_args(Rest, Parser, Parsed) when Rest =/= <<>> ->
    case ephp_parser_expr:expression(Rest, arg_level(Parser), []) of
        {<<")", Rest0/binary>>, Parser0, []} ->
            {Rest0, inc_pos(Parser0), Parsed};
        {<<")", Rest0/binary>>, Parser0, Arg} ->
            {Rest0, inc_pos(Parser0), Parsed ++ [Arg]};
        {<<",", Rest0/binary>>, Parser0, Arg} ->
            call_args(Rest0, inc_pos(Parser0), Parsed ++ [Arg]);
        {<<";", _/binary>>, Parser0, _} ->
            ephp_parser:throw_error(eparse, Parser0, {unexpected, <<";">>});
        {Rest, Parser, _} ->
            ephp_parser:throw_error(eparse, Parser, Rest);
        {Rest0, Parser0, []} ->
            call_args(Rest0, Parser0, Parsed);
        {Rest0, Parser0, Arg} ->
            call_args(Rest0, Parser0, Parsed ++ [Arg])
    end.

st_use(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    st_use(Rest, inc_pos(Parser), Parsed);
st_use(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    st_use(Rest, new_line(Parser), Parsed);
st_use(<<",",Rest/binary>>, Parser, Parsed) ->
    st_use(Rest, inc_pos(Parser), Parsed);
st_use(<<"(",Rest/binary>>, Parser, Parsed) ->
    st_use(Rest, inc_pos(Parser), Parsed);
st_use(<<")",Rest/binary>>, Parser, Parsed) ->
    {Rest, inc_pos(Parser), Parsed};
st_use(<<"&$",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, [Var]} = variable(<<"$",Rest/binary>>, inc_pos(Parser), []),
    st_use(Rest0, Parser0, [add_line(#ref{var=Var}, Parser)|Parsed]);
st_use(<<"$",_/binary>> = Rest, Parser, Parsed) ->
    {Rest0, Parser0, [Var]} = variable(Rest, Parser, []),
    st_use(Rest0, Parser0, [add_line(Var, Parser)|Parsed]).

st_use_or_block(<<SP:8, Rest/binary>>, Parser, Function) when ?IS_SPACE(SP) ->
    st_use_or_block(Rest, inc_pos(Parser), Function);
st_use_or_block(<<SP:8, Rest/binary>>, Parser, Function) when ?IS_NEWLINE(SP) ->
    st_use_or_block(Rest, new_line(Parser), Function);
st_use_or_block(<<"{", Rest/binary>>, Parser, Function) ->
    {Rest0, Parser0, CodeBlock} =
        code(Rest, code_block_level(inc_pos(Parser)), []),
    {Rest0, Parser0, Function#function{code = CodeBlock}};
st_use_or_block(<<U:8,S:8,E:8,SP:8,Rest/binary>>, Parser, Function) when
        ?OR(U,$U,$u) andalso ?OR(S,$S,$s) andalso ?OR(E,$E,$e) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0, Use} = st_use(Rest, Parser, []),
    st_use_or_block(Rest0, Parser0, Function#function{use=Use}).

st_function(<<SP:8, Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    st_function(Rest, inc_pos(Parser), Parsed);
st_function(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    st_function(Rest, new_line(Parser), Parsed);
% TODO if the following char is '(' maybe this is an anon-function
st_function(Rest, Parser, Parsed) ->
    ReturnRef = case remove_spaces(Rest, Parser) of
        {<<"&", Rest0/binary>>, Parser0} -> true;
        {Rest0, Parser0} -> false
    end,
    %% FIXME: we shouldn't let to the user define namespace in function definition
    {Rest1, Parser1, {NS, Name}} = funct_name(Rest0, Parser0, []),
    {<<"(", Rest2/binary>>, Parser2} = remove_spaces(Rest1, Parser1),
    {Rest3, Parser3, Args} = funct_args(Rest2, Parser2, []),
    {Rest4, Parser4, CodeBlock} = code_block(Rest3, Parser3, []),
    Function = add_line(#function{name = Name,
                                  namespace = NS,
                                  args = Args,
                                  code = CodeBlock,
                                  return_ref = ReturnRef}, Parser),
    {Rest4, copy_rowcol(Parser4, Parser), [Function|Parsed]}.

get_ns({[<<>>], Name}, _Parser) -> {[], Name};
get_ns({[], Name}, #parser{use_func_list = FuncList} = Parser) ->
    case lists:keyfind(Name, 1, FuncList) of
        {Name, RealNS} -> {RealNS, Name};
        false -> {ephp_parser:get_ns([], Parser), Name}
    end;
get_ns({NS, Name}, Parser) ->
    {ephp_parser:get_ns(NS, Parser), Name}.

funct_name(<<A:8,Rest/binary>>, Parser, []) when ?IS_ALPHA(A) orelse A =:= $_ orelse A =:= $\\ ->
    funct_name(Rest, inc_pos(Parser), [<<A:8>>]);
funct_name(<<A:8,Rest/binary>>, Parser, [N])
        when ?IS_ALPHA(A) orelse ?IS_NUMBER(A) orelse A =:= $_ orelse A =:= $\\ ->
    funct_name(Rest, inc_pos(Parser), [<<N/binary, A:8>>]);
funct_name(Rest, Parser, [Parsed]) ->
    {Rest, Parser, get_ns(ephp_ns:parse(Parsed), Parser)}.

funct_args(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    funct_args(Rest, inc_pos(Parser), Parsed);
funct_args(<<SP:8,Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    funct_args(Rest, new_line(Parser), Parsed);
funct_args(<<"//", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = ephp_parser:comment_line(Rest, Parser, Parsed),
    funct_args(Rest0, Parser0, Parsed);
funct_args(<<"#", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = ephp_parser:comment_line(Rest, Parser, Parsed),
    funct_args(Rest0, Parser0, Parsed);
funct_args(<<"/*", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = ephp_parser:comment_block(Rest, Parser, Parsed),
    funct_args(Rest0, Parser0, Parsed);
funct_args(<<"&", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, [Var|Parsed0]} = funct_args(Rest, Parser, Parsed),
    {Rest0, Parser0, [add_line(#ref{var = Var}, Parser)|Parsed0]};
funct_args(<<",", Rest/binary>>, Parser, Parsed) ->
    funct_args(Rest, inc_pos(Parser), Parsed);
funct_args(<<")", Rest/binary>>, Parser, Parsed) ->
    {Rest, inc_pos(Parser), lists:reverse(Parsed)};
funct_args(<<A:8, _/binary>> = Rest, Parser, Parsed)
        when ?IS_ALPHA(A) orelse A =:= $_ ->
    {Rest0, Parser0, [#constant{name = Constant}]} =
        ephp_parser:constant(Rest, Parser, []),
    {Rest1, Parser1, Vars} = funct_args(Rest0, Parser0, []),
    case Vars of
        [#ref{var = Var} = Ref|RestVars] ->
            NewRef = Ref#ref{var = Var#variable{data_type = Constant}},
            {Rest1, Parser1, Parsed ++ [NewRef] ++ RestVars};
        [#variable{} = Var|RestVars] ->
            NewVar = Var#variable{data_type = Constant},
            {Rest1, Parser1, Parsed ++ [NewVar] ++ RestVars}
    end;
funct_args(<<"$",_/binary>> = Rest, Parser, Parsed) ->
    {Rest0, Parser0, [Var]} = variable(Rest, Parser, []),
    case remove_spaces(Rest0, Parser0) of
        {<<"=", Rest1/binary>>, Parser1} ->
            NewParser = arg_level(inc_pos(Parser1)),
            {Rest2, Parser2, Default} =
                ephp_parser_expr:expression(Rest1, NewParser, []),
            NewVar = Var#variable{default_value = Default},
            funct_args(Rest2, copy_rowcol(Parser2, Parser), [NewVar|Parsed]);
        {Rest1, Parser1} ->
            funct_args(Rest1, inc_pos(Parser1), [Var|Parsed])
    end.

-module(ephp_parser_string).
-author('manuel@altenwald.com').
-compile([warnings_as_errors, export_all]).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-import(ephp_parser, [
    add_pos/2, new_line/1, add_line/2, variable/3
]).

string(<<"\"",Rest/binary>>, Pos, []) ->
    string_parsed(Rest, Pos, add_line(#text_to_process{text=[]}, Pos));
string(<<"'",Rest/binary>>, Pos, []) ->
    string_fixed(Rest, Pos, add_line(#text{text = <<>>}, Pos));
string(<<"<<<'",Rest/binary>>, {Level,Row,_}=Pos, []) ->
    [W,Rest0] = binary:split(Rest, <<"'\n">>),
    [Text,Rest1] = binary:split(Rest0, <<"\n", W/binary, ";">>),
    NPos = {Level,Row+length(binary:matches(Text,<<"\n">>))+1,1},
    {<<";",Rest1/binary>>, add_pos(NPos,byte_size(W)), add_line(#text{text=Text}, Pos)};
string(<<"<<<",Rest/binary>>, Pos, []) ->
    [W,Rest0] = binary:split(Rest, <<"\n">>),
    Wsize = byte_size(W),
    [RawText,Rest1] = binary:split(Rest0, <<"\n", W/binary, ";">>),
    case heredoc(RawText, add_pos(Pos,Wsize+4), []) of
        {Pos2, [Text]} when is_binary(Text) ->
            {<<";",Rest1/binary>>, Pos2, add_line(#text{text=Text}, Pos)};
        {Pos2, Text} ->
            {<<";",Rest1/binary>>, Pos2,
             add_line(#text_to_process{text=Text}, Pos)}
    end.

heredoc(<<>>, Pos, C) ->
    {Pos, lists:reverse(C)};
heredoc(<<"${",Rest/binary>>, {Level,Row,Col}, C) ->
    {Rest0, {_,Row0,Col0}, [Var]} = variable(Rest, {enclosed,Row,Col+2}, []),
    heredoc(Rest0, {Level,Row0,Col0}, [Var|C]);
heredoc(<<"{$",Rest/binary>>, {Level,Row,Col}, C) ->
    {Rest0, {_,Row0,Col0}, [Var]} = variable(Rest, {enclosed,Row,Col+2}, []),
    heredoc(Rest0, {Level,Row0,Col0}, [Var|C]);
heredoc(<<"$",Rest/binary>>, {Level,Row,Col}, C) ->
    {Rest0, {_,Row0,Col0}, [Var]} = variable(Rest, {Level,Row,Col+1}, []),
    heredoc(Rest0, {Level,Row0,Col0}, [Var|C]);
heredoc(<<"\n",Rest/binary>>, Pos, [C|R]) when is_binary(C) ->
    heredoc(Rest, new_line(Pos), [<<C/binary, "\n">>|R]);
heredoc(<<A/utf8,Rest/binary>>, Pos, [C|R]) when is_binary(C) ->
    heredoc(Rest, add_pos(Pos,1), [<<C/binary, A/utf8>>|R]);
heredoc(Rest, Pos, []) ->
    heredoc(Rest, Pos, [<<>>]);
heredoc(Rest, Pos, [C|_]=S) when not is_binary(C) ->
    heredoc(Rest, Pos, [<<>>|S]).

string_fixed(<<"\\\\",Rest/binary>>, Pos, #text{text=C}=S) ->
    string_fixed(Rest, add_pos(Pos,1), S#text{text = <<C/binary, "\\\\">>});
string_fixed(<<"\\'",Rest/binary>>, Pos, #text{text=C}=S) ->
    string_fixed(Rest, add_pos(Pos,1), S#text{text = <<C/binary, "\\'">>});
string_fixed(<<"'",Rest/binary>>, Pos, Parsed) ->
    {Rest, add_pos(Pos,1), Parsed};
string_fixed(<<"\n",Rest/binary>>, Pos, #text{text=C}=S) ->
    string_fixed(Rest, new_line(Pos), S#text{text = <<C/binary, "\n">>});
string_fixed(<<A/utf8,Rest/binary>>, Pos, #text{text=C}=S) ->
    string_fixed(Rest, add_pos(Pos,1), S#text{text = <<C/binary, A/utf8>>}).

string_parsed(<<"\\\\",Rest/binary>>, Pos, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "\\\\">>|R]},
    string_parsed(Rest, add_pos(Pos,1), NewText);
string_parsed(<<"\\\"",Rest/binary>>, Pos, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "\\\"">>|R]},
    string_parsed(Rest, add_pos(Pos,1), NewText);
string_parsed(<<"\\$",Rest/binary>>, Pos, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "$">>|R]},
    string_parsed(Rest, add_pos(Pos,2), NewText);
string_parsed(<<"\"",Rest/binary>>, Pos, #text_to_process{text=[C]}=S)
        when is_binary(C) ->
    {Rest, add_pos(Pos,1), #text{text=C, line=S#text_to_process.line}};
string_parsed(<<"\"",Rest/binary>>, Pos, #text_to_process{text=C}=S) ->
    {Rest, add_pos(Pos,1), S#text_to_process{text=lists:reverse(C)}};
string_parsed(<<"\\n",Rest/binary>>, Pos, #text_to_process{text=T}=S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\n>>|R];
        T ->
            [<<$\n>>|T]
    end,
    string_parsed(Rest, add_pos(Pos,2), S#text_to_process{text=NewT});
string_parsed(<<"\\t",Rest/binary>>, Pos, #text_to_process{text=T}=S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\t>>|R];
        T ->
            [<<$\t>>|T]
    end,
    string_parsed(Rest, add_pos(Pos,2), S#text_to_process{text=NewT});
string_parsed(<<"\\r",Rest/binary>>, Pos, #text_to_process{text=T}=S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\r>>|R];
        T ->
            [<<$\r>>|T]
    end,
    string_parsed(Rest, add_pos(Pos,2), S#text_to_process{text=NewT});
string_parsed(<<"\n",Rest/binary>>, Pos, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "\n">>|R]},
    string_parsed(Rest, new_line(Pos), NewText);
string_parsed(<<"${",Rest/binary>>, {Level,Row,Col},
              #text_to_process{text=C}=S) ->
    {Rest0, {_,Row0,Col0}, [Var]} = variable(Rest, {enclosed,Row,Col+2}, []),
    NewText = S#text_to_process{text=[Var|C]},
    string_parsed(Rest0, {Level,Row0,Col0}, NewText);
string_parsed(<<"{$",Rest/binary>>, {Level,Row,Col},
              #text_to_process{text=C}=S) ->
    {Rest0, {_,Row0,Col0}, [Var]} = variable(Rest, {enclosed,Row,Col+2}, []),
    NewText = S#text_to_process{text=[Var|C]},
    string_parsed(Rest0, {Level,Row0,Col0}, NewText);
string_parsed(<<"$",Rest/binary>>, {Level,Row,Col},
              #text_to_process{text=C}=S) ->
    {Rest0, {_,Row0,Col0}, [Var]} = variable(Rest, {unclosed,Row,Col+1}, []),
    NewText = S#text_to_process{text=[Var|C]},
    string_parsed(Rest0, {Level,Row0,Col0}, NewText);
string_parsed(<<A/utf8,Rest/binary>>, Pos, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, A/utf8>>|R]},
    string_parsed(Rest, add_pos(Pos,1), NewText);
string_parsed(Rest, Pos, #text_to_process{text=C}=S)
        when not is_binary(C) ->
    string_parsed(Rest, Pos, S#text_to_process{text=[<<>>|C]}).

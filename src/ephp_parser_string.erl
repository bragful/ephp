-module(ephp_parser_string).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-export([string/3]).

-import(ephp_parser, [
    add_pos/2, new_line/1, add_line/2, variable/3, throw_error/3, new_line/2,
    enclosed_level/1, unclosed_level/1, copy_level/2
]).

string(<<"\"", Rest/binary>>, Pos, []) ->
    string_parsed(Rest, add_pos(Pos, 1),
                  add_line(#text_to_process{text = []}, Pos));
string(<<"'", Rest/binary>>, Pos, []) ->
    string_fixed(Rest, add_pos(Pos, 1), add_line(#text{text = <<>>}, Pos));
string(<<"`", Rest/binary>>, Pos, []) ->
    string_command(Rest, add_pos(Pos, 1), add_line(#command{text = []}, Pos));
string(<<"<<<'", Rest/binary>>, Pos, []) ->
    [W, Rest0] = binary:split(Rest, <<"'\n">>),
    [Text, Rest1] = binary:split(Rest0, <<"\n", W/binary, ";">>),
    NPos = new_line(Pos, length(binary:matches(Text, <<"\n">>)) + 1),
    {<<";", Rest1/binary>>, add_pos(NPos, byte_size(W)),
     add_line(#text{text = Text}, Pos)};
string(<<"<<<", Rest/binary>>, Pos, []) ->
    [W, Rest0] = binary:split(Rest, <<"\n">>),
    Wsize = byte_size(W),
    [RawText, Rest1] = binary:split(Rest0, <<"\n", W/binary, ";">>),
    {Pos2, Text} = heredoc(RawText, new_line(Pos), []),
    {<<";", Rest1/binary>>, add_pos(new_line(Pos2), Wsize), Text}.

add_text(C, Pos, []) ->
    [add_line(#text_to_process{text = [C]}, Pos)];
add_text(C, _Pos, [#text_to_process{text = [T|TN]} = T2P]) when is_binary(C)
                                                        andalso is_binary(T) ->
    [T2P#text_to_process{text = [<<T/binary, C/binary>>|TN]}];
add_text(C, _Pos, [#text_to_process{text = TN} = T2P]) ->
    [T2P#text_to_process{text = [C|TN]}].

heredoc(<<>>, Pos, [#text_to_process{text = Text} = T2P]) ->
    {Pos, [T2P#text_to_process{text = lists:reverse(Text)}]};
heredoc(<<"\\x", HexBin1:8, HexBin2:8, Rest/binary>>, Pos, Text)
        when ?IS_HEX(HexBin1) andalso ?IS_HEX(HexBin2) ->
    Data = binary_to_integer(<<HexBin1:8, HexBin2:8>>, 16),
    heredoc(Rest, add_pos(Pos, 4), add_text(<<Data:8>>, Pos, Text));
heredoc(<<"\\x", HexBin1:8, Rest/binary>>, Pos, Text) when ?IS_HEX(HexBin1) ->
    Data = binary_to_integer(<<HexBin1:8>>, 16),
    heredoc(Rest, add_pos(Pos, 4), add_text(<<Data:8>>, Pos, Text));
heredoc(<<"\\", OctBin1:8, OctBin2:8, OctBin3:8, Rest/binary>>, Pos, Text)
        when ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2)
        andalso ?IS_OCT(OctBin3) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8, OctBin3:8>>, 8),
    heredoc(Rest, add_pos(Pos, 4), add_text(<<Data:8>>, Pos, Text));
heredoc(<<"\\", OctBin1:8, OctBin2:8, Rest/binary>>, Pos, Text)
        when ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8>>, 8),
    heredoc(Rest, add_pos(Pos, 4), add_text(<<Data:8>>, Pos, Text));
heredoc(<<"\\", OctBin:8, Rest/binary>>, Pos, Text) when ?IS_OCT(OctBin) ->
    Data = binary_to_integer(<<OctBin:8>>, 8),
    heredoc(Rest, add_pos(Pos, 4), add_text(<<Data:8>>, Pos, Text));
heredoc(<<"${",Rest/binary>>, Pos, Text) ->
    NewPos = ephp_parser:enclosed_level(add_pos(Pos, 2)),
    {<<"}", Rest0/binary>>, Pos0, [Var]} = variable(Rest, NewPos, []),
    heredoc(Rest0, ephp_parser:copy_level(Pos, Pos0),
            add_text(Var, Pos, Text));
heredoc(<<"{$",Rest/binary>>, Pos, Text) ->
    NewPos = ephp_parser:enclosed_level(add_pos(Pos, 2)),
    {<<"}", Rest0/binary>>, Pos0, [Var]} = variable(Rest, NewPos, []),
    heredoc(Rest0, ephp_parser:copy_level(Pos, Pos0),
            add_text(Var, Pos, Text));
heredoc(<<"$", A:8, B/binary>>, Pos, Text) when ?IS_ALPHA(A) orelse A =:= $_ ->
    Rest = <<A:8, B/binary>>,
    {Rest0, Pos0, [Var]} = variable(Rest, add_pos(Pos, 1), []),
    heredoc(Rest0, ephp_parser:copy_level(Pos, Pos0),
            add_text(Var, Pos, Text));
heredoc(<<"\n",Rest/binary>>, Pos, Text) ->
    heredoc(Rest, new_line(Pos), add_text(<<"\n">>, Pos, Text));
heredoc(<<A/utf8,Rest/binary>>, Pos, Text) ->
    heredoc(Rest, add_pos(Pos, 1), add_text(<<A/utf8>>, Pos, Text)).

string_fixed(<<>>, {L,_,_}, #text{line={{line,R},{column,C}}}) ->
    throw_error(eparse, {L,R,C}, <<>>);
string_fixed(<<"\\\\",Rest/binary>>, Pos, #text{text=C}=S) ->
    string_fixed(Rest, add_pos(Pos,1), S#text{text = <<C/binary, "\\\\">>});
string_fixed(<<"\\'",Rest/binary>>, Pos, #text{text=C}=S) ->
    string_fixed(Rest, add_pos(Pos,1), S#text{text = <<C/binary, "'">>});
string_fixed(<<"'",Rest/binary>>, Pos, Parsed) ->
    {Rest, add_pos(Pos,1), Parsed};
string_fixed(<<"\n",Rest/binary>>, Pos, #text{text=C}=S) ->
    string_fixed(Rest, new_line(Pos), S#text{text = <<C/binary, "\n">>});
string_fixed(<<A/utf8,Rest/binary>>, Pos, #text{text=C}=S) ->
    string_fixed(Rest, add_pos(Pos,1), S#text{text = <<C/binary, A/utf8>>}).

string_parsed(<<>>, Pos, _Text) ->
    throw_error(eparse, Pos, <<>>);
string_parsed(<<"\\\\",Rest/binary>>, Pos, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "\\\\">>|R]},
    string_parsed(Rest, add_pos(Pos,1), NewText);
string_parsed(<<"\\\"",Rest/binary>>, Pos, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "\"">>|R]},
    string_parsed(Rest, add_pos(Pos,1), NewText);
string_parsed(<<"\\x", HexBin1:8, HexBin2:8, Rest/binary>>, Pos,
              #text_to_process{text=[C|R]}=S)
        when is_binary(C)
        andalso ?IS_HEX(HexBin1)
        andalso ?IS_HEX(HexBin2) ->
    Data = binary_to_integer(<<HexBin1:8, HexBin2:8>>, 16),
    NewText = S#text_to_process{text = [<<C/binary, Data:8>>|R]},
    string_parsed(Rest, add_pos(Pos, 4), NewText);
string_parsed(<<"\\x", HexBin1:8, Rest/binary>>, Pos,
              #text_to_process{text=[C|R]}=S)
        when is_binary(C)
        andalso ?IS_HEX(HexBin1) ->
    Data = binary_to_integer(<<HexBin1:8>>, 16),
    NewText = S#text_to_process{text = [<<C/binary, Data:8>>|R]},
    string_parsed(Rest, add_pos(Pos, 4), NewText);
string_parsed(<<"\\", OctBin1:8, OctBin2:8, OctBin3:8, Rest/binary>>, Pos,
              #text_to_process{text=[C|R]}=S)
        when is_binary(C) andalso ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2)
        andalso ?IS_OCT(OctBin3) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8, OctBin3:8>>, 8),
    NewText = S#text_to_process{text = [<<C/binary, Data:8>>|R]},
    string_parsed(Rest, add_pos(Pos, 4), NewText);
string_parsed(<<"\\", OctBin1:8, OctBin2:8, Rest/binary>>, Pos,
              #text_to_process{text=[C|R]}=S)
        when is_binary(C) andalso ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8>>, 8),
    NewText = S#text_to_process{text = [<<C/binary, Data:8>>|R]},
    string_parsed(Rest, add_pos(Pos, 4), NewText);
string_parsed(<<"\\", OctBin:8, Rest/binary>>, Pos,
              #text_to_process{text=[C|R]}=S)
        when is_binary(C) andalso ?IS_OCT(OctBin) ->
    Data = binary_to_integer(<<OctBin:8>>, 8),
    NewText = S#text_to_process{text = [<<C/binary, Data:8>>|R]},
    string_parsed(Rest, add_pos(Pos, 4), NewText);
string_parsed(<<"\\$",Rest/binary>>, Pos, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "$">>|R]},
    string_parsed(Rest, add_pos(Pos,2), NewText);
string_parsed(<<"\"",Rest/binary>>, Pos, #text_to_process{text=[C]}=S)
        when is_binary(C) ->
    {Rest, add_pos(Pos,1), #text{text=C, line=S#text_to_process.line}};
string_parsed(<<"\"",Rest/binary>>, Pos, #text_to_process{text=[]}=S) ->
    {Rest, add_pos(Pos,1), #text{text = <<>>, line=S#text_to_process.line}};
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
string_parsed(<<"${", Rest/binary>>, {Level, Row, Col},
              #text_to_process{text = C} = S) ->
    {<<"}", Rest0/binary>>, {_, Row0, Col0}, [Var]} =
        variable(Rest, {enclosed, Row, Col + 2}, []),
    NewText = S#text_to_process{text = [Var|C]},
    string_parsed(Rest0, {Level, Row0, Col0 + 1}, NewText);
string_parsed(<<"{$",Rest/binary>>, {Level, Row, Col},
              #text_to_process{text = C} = S) ->
    {<<"}", Rest0/binary>>, {_, Row0, Col0}, [Var]} =
        variable(Rest, {enclosed, Row, Col + 2}, []),
    NewText = S#text_to_process{text = [Var|C]},
    string_parsed(Rest0, {Level, Row0, Col0 + 1}, NewText);
string_parsed(<<"$",A:8,Rest/binary>>, {Level,Row,Col},
              #text_to_process{text=C}=S) when ?IS_ALPHA(A) orelse A =:= $_ ->
    {Rest0, {_,Row0,Col0}, [Var]} =
        variable(<<A:8,Rest/binary>>, {unclosed,Row,Col+1}, []),
    NewText = S#text_to_process{text=[Var|C]},
    string_parsed(Rest0, {Level,Row0,Col0}, NewText);
string_parsed(<<A/utf8,Rest/binary>>, Pos, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, A/utf8>>|R]},
    string_parsed(Rest, add_pos(Pos,1), NewText);
string_parsed(Rest, Pos, #text_to_process{text=C}=S)
        when not is_binary(C) ->
    string_parsed(Rest, Pos, S#text_to_process{text=[<<>>|C]}).


string_command(<<>>, Pos, _Text) ->
    throw_error(eparse, Pos, <<>>);
string_command(<<"\\\\",Rest/binary>>, Pos, #command{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#command{text = [<<C/binary, "\\\\">>|R]},
    string_command(Rest, add_pos(Pos,1), NewText);
string_command(<<"\\`",Rest/binary>>, Pos, #command{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#command{text = [<<C/binary, "`">>|R]},
    string_command(Rest, add_pos(Pos,1), NewText);
string_command(<<"\\x", HexBin1:8, HexBin2:8, Rest/binary>>, Pos,
              #command{text=[C|R]}=S)
        when is_binary(C)
        andalso ?IS_HEX(HexBin1)
        andalso ?IS_HEX(HexBin2) ->
    Data = binary_to_integer(<<HexBin1:8, HexBin2:8>>, 16),
    NewText = S#command{text = [<<C/binary, Data:8>>|R]},
    string_command(Rest, add_pos(Pos, 4), NewText);
string_command(<<"\\x", HexBin1:8, Rest/binary>>, Pos,
              #command{text=[C|R]}=S)
        when is_binary(C)
        andalso ?IS_HEX(HexBin1) ->
    Data = binary_to_integer(<<HexBin1:8>>, 16),
    NewText = S#command{text = [<<C/binary, Data:8>>|R]},
    string_command(Rest, add_pos(Pos, 4), NewText);
string_command(<<"\\", OctBin1:8, OctBin2:8, OctBin3:8, Rest/binary>>, Pos,
              #command{text=[C|R]}=S)
        when is_binary(C) andalso ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2)
        andalso ?IS_OCT(OctBin3) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8, OctBin3:8>>, 8),
    NewText = S#command{text = [<<C/binary, Data:8>>|R]},
    string_command(Rest, add_pos(Pos, 4), NewText);
string_command(<<"\\", OctBin1:8, OctBin2:8, Rest/binary>>, Pos,
              #command{text=[C|R]}=S)
        when is_binary(C) andalso ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8>>, 8),
    NewText = S#command{text = [<<C/binary, Data:8>>|R]},
    string_command(Rest, add_pos(Pos, 4), NewText);
string_command(<<"\\", OctBin:8, Rest/binary>>, Pos,
              #command{text=[C|R]}=S)
        when is_binary(C) andalso ?IS_OCT(OctBin) ->
    Data = binary_to_integer(<<OctBin:8>>, 8),
    NewText = S#command{text = [<<C/binary, Data:8>>|R]},
    string_command(Rest, add_pos(Pos, 4), NewText);
string_command(<<"\\$",Rest/binary>>, Pos, #command{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#command{text = [<<C/binary, "$">>|R]},
    string_command(Rest, add_pos(Pos,2), NewText);
string_command(<<"`", Rest/binary>>, Pos, #command{text = C, line = Line}) ->
    Text = #text_to_process{text = lists:reverse(C), line = Line},
    {Rest, add_pos(Pos, 1), #call{name = <<"shell_exec">>,
                                  args = [Text], line = Line}};
string_command(<<"\\n", Rest/binary>>, Pos, #command{text = T} = S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\n>>|R];
        T ->
            [<<$\n>>|T]
    end,
    string_command(Rest, add_pos(Pos, 2), S#command{text = NewT});
string_command(<<"\\t", Rest/binary>>, Pos, #command{text = T} = S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\t>>|R];
        T ->
            [<<$\t>>|T]
    end,
    string_command(Rest, add_pos(Pos,2), S#command{text = NewT});
string_command(<<"\\r", Rest/binary>>, Pos, #command{text = T} = S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\r>>|R];
        T ->
            [<<$\r>>|T]
    end,
    string_command(Rest, add_pos(Pos, 2), S#command{text = NewT});
string_command(<<"\n", Rest/binary>>, Pos, #command{text = [C|R]} = S)
        when is_binary(C) ->
    NewText = S#command{text = [<<C/binary, "\n">>|R]},
    string_command(Rest, new_line(Pos), NewText);
string_command(<<"${", Rest/binary>>, Pos, #command{text = C} = S) ->
    NewPos = enclosed_level(add_pos(Pos, 2)),
    {<<"}", Rest0/binary>>, Pos0, [Var]} = variable(Rest, NewPos, []),
    NewText = S#command{text = [Var|C]},
    string_command(Rest0, copy_level(Pos, add_pos(Pos0, 1)), NewText);
string_command(<<"{$",Rest/binary>>, Pos, #command{text = C} = S) ->
    NewPos = enclosed_level(add_pos(Pos, 2)),
    {<<"}", Rest0/binary>>, Pos0, [Var]} = variable(Rest, NewPos, []),
    NewText = S#command{text = [Var|C]},
    string_command(Rest0, add_pos(copy_level(Pos, Pos0), 1), NewText);
string_command(<<"$", A:8, Rest/binary>>, Pos,
               #command{text=C}=S) when ?IS_ALPHA(A) orelse A =:= $_ ->
    NewPos = unclosed_level(add_pos(Pos, 1)),
    {Rest0, Pos0, [Var]} = variable(<<A:8, Rest/binary>>, NewPos, []),
    NewText = S#command{text = [Var|C]},
    string_command(Rest0, copy_level(Pos, Pos0), NewText);
string_command(<<A/utf8, Rest/binary>>, Pos, #command{text = [C|R]} = S)
        when is_binary(C) ->
    NewText = S#command{text = [<<C/binary, A/utf8>>|R]},
    string_command(Rest, add_pos(Pos, 1), NewText);
string_command(Rest, Pos, #command{text = C} = S)
        when not is_binary(C) ->
    string_command(Rest, Pos, S#command{text = [<<>>|C]}).

-module(ephp_parser_string).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-export([string/3]).

-import(ephp_parser, [
    add_pos/2, new_line/1, add_line/2, variable/3, throw_error/3, new_line/2,
    enclosed_level/1, unclosed_level/1, copy_level/2, inc_pos/1
]).

string(<<"\"", Rest/binary>>, Parser, []) ->
    string_parsed(Rest, inc_pos(Parser),
                  add_line(#text_to_process{text = []}, Parser));
string(<<"'", Rest/binary>>, Parser, []) ->
    string_fixed(Rest, inc_pos(Parser), add_line(#text{text = <<>>}, Parser));
string(<<"`", Rest/binary>>, Parser, []) ->
    string_command(Rest, inc_pos(Parser), add_line(#command{text = []}, Parser));
string(<<"<<<'", Rest/binary>>, Parser, []) ->
    [W, Rest0] = binary:split(Rest, <<"'\n">>),
    [Text, Rest1] = binary:split(Rest0, <<"\n", W/binary, ";">>),
    NewParser = new_line(Parser, length(binary:matches(Text, <<"\n">>)) + 1),
    {<<";", Rest1/binary>>, add_pos(NewParser, byte_size(W)),
     add_line(#text{text = Text}, Parser)};
string(<<"<<<", Rest/binary>>, Parser, []) ->
    [W, Rest0] = binary:split(Rest, <<"\n">>),
    Wsize = byte_size(W),
    [RawText, Rest1] = binary:split(Rest0, <<"\n", W/binary, ";">>),
    {Parser2, Text} = heredoc(RawText, new_line(Parser), []),
    {<<";", Rest1/binary>>, add_pos(new_line(Parser2), Wsize), Text}.

add_text(C, Parser, []) ->
    [add_line(#text_to_process{text = [C]}, Parser)];
add_text(C, _Parser, [#text_to_process{text = [T|TN]} = T2P]) when is_binary(C)
                                                           andalso is_binary(T) ->
    [T2P#text_to_process{text = [<<T/binary, C/binary>>|TN]}];
add_text(C, _Parser, [#text_to_process{text = TN} = T2P]) ->
    [T2P#text_to_process{text = [C|TN]}].

heredoc(<<>>, Parser, [#text_to_process{text = Text} = T2P]) ->
    {Parser, [T2P#text_to_process{text = lists:reverse(Text)}]};
heredoc(<<"\\x", HexBin1:8, HexBin2:8, Rest/binary>>, Parser, Text)
        when ?IS_HEX(HexBin1) andalso ?IS_HEX(HexBin2) ->
    Data = binary_to_integer(<<HexBin1:8, HexBin2:8>>, 16),
    heredoc(Rest, add_pos(Parser, 4), add_text(<<Data:8>>, Parser, Text));
heredoc(<<"\\x", HexBin1:8, Rest/binary>>, Parser, Text) when ?IS_HEX(HexBin1) ->
    Data = binary_to_integer(<<HexBin1:8>>, 16),
    heredoc(Rest, add_pos(Parser, 4), add_text(<<Data:8>>, Parser, Text));
heredoc(<<"\\", OctBin1:8, OctBin2:8, OctBin3:8, Rest/binary>>, Parser, Text)
        when ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2)
        andalso ?IS_OCT(OctBin3) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8, OctBin3:8>>, 8),
    heredoc(Rest, add_pos(Parser, 4), add_text(<<Data:8>>, Parser, Text));
heredoc(<<"\\", OctBin1:8, OctBin2:8, Rest/binary>>, Parser, Text)
        when ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8>>, 8),
    heredoc(Rest, add_pos(Parser, 4), add_text(<<Data:8>>, Parser, Text));
heredoc(<<"\\", OctBin:8, Rest/binary>>, Parser, Text) when ?IS_OCT(OctBin) ->
    Data = binary_to_integer(<<OctBin:8>>, 8),
    heredoc(Rest, add_pos(Parser, 4), add_text(<<Data:8>>, Parser, Text));
heredoc(<<"${",Rest/binary>>, Parser, Text) ->
    NewParser = ephp_parser:enclosed_level(add_pos(Parser, 2)),
    {<<"}", Rest0/binary>>, Parser0, [Var]} = variable(Rest, NewParser, []),
    heredoc(Rest0, ephp_parser:copy_level(Parser, Parser0),
            add_text(Var, Parser, Text));
heredoc(<<"{$",Rest/binary>>, Parser, Text) ->
    NewParser = ephp_parser:enclosed_level(add_pos(Parser, 2)),
    {<<"}", Rest0/binary>>, Parser0, [Var]} = variable(Rest, NewParser, []),
    heredoc(Rest0, ephp_parser:copy_level(Parser, Parser0),
            add_text(Var, Parser, Text));
heredoc(<<"$", A:8, B/binary>>, Parser, Text) when ?IS_ALPHA(A) orelse A =:= $_ ->
    Rest = <<A:8, B/binary>>,
    {Rest0, Parser0, [Var]} = variable(Rest, inc_pos(Parser), []),
    heredoc(Rest0, ephp_parser:copy_level(Parser, Parser0),
            add_text(Var, Parser, Text));
heredoc(<<"\n",Rest/binary>>, Parser, Text) ->
    heredoc(Rest, new_line(Parser), add_text(<<"\n">>, Parser, Text));
heredoc(<<A/utf8,Rest/binary>>, Parser, Text) ->
    heredoc(Rest, inc_pos(Parser), add_text(<<A/utf8>>, Parser, Text)).

string_fixed(<<>>, Parser, #text{line = {{line,R},{column,C}}}) ->
    throw_error(eparse, Parser#parser{row = R, col = C}, <<>>);
string_fixed(<<"\\\\",Rest/binary>>, Parser, #text{text=C}=S) ->
    string_fixed(Rest, inc_pos(Parser), S#text{text = <<C/binary, "\\\\">>});
string_fixed(<<"\\'",Rest/binary>>, Parser, #text{text=C}=S) ->
    string_fixed(Rest, inc_pos(Parser), S#text{text = <<C/binary, "'">>});
string_fixed(<<"'",Rest/binary>>, Parser, Parsed) ->
    {Rest, inc_pos(Parser), Parsed};
string_fixed(<<"\n",Rest/binary>>, Parser, #text{text=C}=S) ->
    string_fixed(Rest, new_line(Parser), S#text{text = <<C/binary, "\n">>});
string_fixed(<<A/utf8,Rest/binary>>, Parser, #text{text=C}=S) ->
    string_fixed(Rest, inc_pos(Parser), S#text{text = <<C/binary, A/utf8>>}).

string_parsed(<<>>, Parser, _Text) ->
    throw_error(eparse, Parser, <<>>);
string_parsed(<<"\\\\",Rest/binary>>, Parser, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "\\\\">>|R]},
    string_parsed(Rest, inc_pos(Parser), NewText);
string_parsed(<<"\\\"",Rest/binary>>, Parser, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "\"">>|R]},
    string_parsed(Rest, inc_pos(Parser), NewText);
string_parsed(<<"\\x", HexBin1:8, HexBin2:8, Rest/binary>>, Parser,
              #text_to_process{text=[C|R]}=S)
        when is_binary(C)
        andalso ?IS_HEX(HexBin1)
        andalso ?IS_HEX(HexBin2) ->
    Data = binary_to_integer(<<HexBin1:8, HexBin2:8>>, 16),
    NewText = S#text_to_process{text = [<<C/binary, Data:8>>|R]},
    string_parsed(Rest, add_pos(Parser, 4), NewText);
string_parsed(<<"\\x", HexBin1:8, Rest/binary>>, Parser,
              #text_to_process{text=[C|R]}=S)
        when is_binary(C)
        andalso ?IS_HEX(HexBin1) ->
    Data = binary_to_integer(<<HexBin1:8>>, 16),
    NewText = S#text_to_process{text = [<<C/binary, Data:8>>|R]},
    string_parsed(Rest, add_pos(Parser, 4), NewText);
string_parsed(<<"\\", OctBin1:8, OctBin2:8, OctBin3:8, Rest/binary>>, Parser,
              #text_to_process{text=[C|R]}=S)
        when is_binary(C) andalso ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2)
        andalso ?IS_OCT(OctBin3) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8, OctBin3:8>>, 8),
    NewText = S#text_to_process{text = [<<C/binary, Data:8>>|R]},
    string_parsed(Rest, add_pos(Parser, 4), NewText);
string_parsed(<<"\\", OctBin1:8, OctBin2:8, Rest/binary>>, Parser,
              #text_to_process{text=[C|R]}=S)
        when is_binary(C) andalso ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8>>, 8),
    NewText = S#text_to_process{text = [<<C/binary, Data:8>>|R]},
    string_parsed(Rest, add_pos(Parser, 4), NewText);
string_parsed(<<"\\", OctBin:8, Rest/binary>>, Parser,
              #text_to_process{text=[C|R]}=S)
        when is_binary(C) andalso ?IS_OCT(OctBin) ->
    Data = binary_to_integer(<<OctBin:8>>, 8),
    NewText = S#text_to_process{text = [<<C/binary, Data:8>>|R]},
    string_parsed(Rest, add_pos(Parser, 4), NewText);
string_parsed(<<"\\$",Rest/binary>>, Parser, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "$">>|R]},
    string_parsed(Rest, add_pos(Parser, 2), NewText);
string_parsed(<<"\"",Rest/binary>>, Parser, #text_to_process{text=[C]}=S)
        when is_binary(C) ->
    {Rest, inc_pos(Parser), #text{text=C, line=S#text_to_process.line}};
string_parsed(<<"\"",Rest/binary>>, Parser, #text_to_process{text=[]}=S) ->
    {Rest, inc_pos(Parser), #text{text = <<>>, line=S#text_to_process.line}};
string_parsed(<<"\"",Rest/binary>>, Parser, #text_to_process{text=C}=S) ->
    {Rest, inc_pos(Parser), S#text_to_process{text=lists:reverse(C)}};
string_parsed(<<"\\n",Rest/binary>>, Parser, #text_to_process{text=T}=S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\n>>|R];
        T ->
            [<<$\n>>|T]
    end,
    string_parsed(Rest, add_pos(Parser, 2), S#text_to_process{text=NewT});
string_parsed(<<"\\t",Rest/binary>>, Parser, #text_to_process{text=T}=S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\t>>|R];
        T ->
            [<<$\t>>|T]
    end,
    string_parsed(Rest, add_pos(Parser, 2), S#text_to_process{text=NewT});
string_parsed(<<"\\r",Rest/binary>>, Parser, #text_to_process{text=T}=S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\r>>|R];
        T ->
            [<<$\r>>|T]
    end,
    string_parsed(Rest, add_pos(Parser, 2), S#text_to_process{text=NewT});
string_parsed(<<"\n",Rest/binary>>, Parser, #text_to_process{text=[C|R]}=S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, "\n">>|R]},
    string_parsed(Rest, new_line(Parser), NewText);
string_parsed(<<"${", Rest/binary>>, #parser{col = Col} = Parser,
              #text_to_process{text = C} = S) ->
    {<<"}", Rest0/binary>>, #parser{row = Row0, col = Col0}, [Var]} =
        variable(Rest, Parser#parser{level = enclosed, col = Col + 2}, []),
    NewText = S#text_to_process{text = [Var|C]},
    string_parsed(Rest0, Parser#parser{row = Row0, col = Col0 + 1}, NewText);
string_parsed(<<"{$",Rest/binary>>, #parser{col = Col} = Parser,
              #text_to_process{text = C} = S) ->
    {<<"}", Rest0/binary>>, #parser{row = Row0, col = Col0}, [Var]} =
        variable(Rest, Parser#parser{level = enclosed, col = Col + 2}, []),
    NewText = S#text_to_process{text = [Var|C]},
    string_parsed(Rest0, Parser#parser{row = Row0, col = Col0 + 1}, NewText);
string_parsed(<<"$",A:8,Rest/binary>>, #parser{col = Col} = Parser,
              #text_to_process{text = C} = S) when ?IS_ALPHA(A) orelse A =:= $_ ->
    {Rest0, #parser{row = Row0, col = Col0}, [Var]} =
        variable(<<A:8,Rest/binary>>, Parser#parser{level = unclosed, col = Col+1}, []),
    NewText = S#text_to_process{text = [Var|C]},
    string_parsed(Rest0, Parser#parser{row = Row0, col = Col0}, NewText);
string_parsed(<<A/utf8, Rest/binary>>, Parser, #text_to_process{text = [C|R]} = S)
        when is_binary(C) ->
    NewText = S#text_to_process{text = [<<C/binary, A/utf8>>|R]},
    string_parsed(Rest, inc_pos(Parser), NewText);
string_parsed(Rest, Parser, #text_to_process{text=C}=S)
        when not is_binary(C) ->
    string_parsed(Rest, Parser, S#text_to_process{text=[<<>>|C]}).


string_command(<<>>, Parser, _Text) ->
    throw_error(eparse, Parser, <<>>);
string_command(<<"\\\\", Rest/binary>>, Parser, #command{text = [C|R]} = S)
        when is_binary(C) ->
    NewText = S#command{text = [<<C/binary, "\\\\">>|R]},
    string_command(Rest, inc_pos(Parser), NewText);
string_command(<<"\\`",Rest/binary>>, Parser, #command{text = [C|R]} = S)
        when is_binary(C) ->
    NewText = S#command{text = [<<C/binary, "`">>|R]},
    string_command(Rest, inc_pos(Parser), NewText);
string_command(<<"\\x", HexBin1:8, HexBin2:8, Rest/binary>>, Parser,
              #command{text = [C|R]} = S)
        when is_binary(C)
        andalso ?IS_HEX(HexBin1)
        andalso ?IS_HEX(HexBin2) ->
    Data = binary_to_integer(<<HexBin1:8, HexBin2:8>>, 16),
    NewText = S#command{text = [<<C/binary, Data:8>>|R]},
    string_command(Rest, add_pos(Parser, 4), NewText);
string_command(<<"\\x", HexBin1:8, Rest/binary>>, Parser,
              #command{text=[C|R]}=S)
        when is_binary(C)
        andalso ?IS_HEX(HexBin1) ->
    Data = binary_to_integer(<<HexBin1:8>>, 16),
    NewText = S#command{text = [<<C/binary, Data:8>>|R]},
    string_command(Rest, add_pos(Parser, 4), NewText);
string_command(<<"\\", OctBin1:8, OctBin2:8, OctBin3:8, Rest/binary>>, Parser,
              #command{text = [C|R]} = S)
        when is_binary(C) andalso ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2)
        andalso ?IS_OCT(OctBin3) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8, OctBin3:8>>, 8),
    NewText = S#command{text = [<<C/binary, Data:8>>|R]},
    string_command(Rest, add_pos(Parser, 4), NewText);
string_command(<<"\\", OctBin1:8, OctBin2:8, Rest/binary>>, Parser,
              #command{text = [C|R]} = S)
        when is_binary(C) andalso ?IS_OCT(OctBin1) andalso ?IS_OCT(OctBin2) ->
    Data = binary_to_integer(<<OctBin1:8, OctBin2:8>>, 8),
    NewText = S#command{text = [<<C/binary, Data:8>>|R]},
    string_command(Rest, add_pos(Parser, 4), NewText);
string_command(<<"\\", OctBin:8, Rest/binary>>, Parser,
              #command{text = [C|R]} = S)
        when is_binary(C) andalso ?IS_OCT(OctBin) ->
    Data = binary_to_integer(<<OctBin:8>>, 8),
    NewText = S#command{text = [<<C/binary, Data:8>>|R]},
    string_command(Rest, add_pos(Parser, 4), NewText);
string_command(<<"\\$",Rest/binary>>, Parser, #command{text = [C|R]} = S)
        when is_binary(C) ->
    NewText = S#command{text = [<<C/binary, "$">>|R]},
    string_command(Rest, add_pos(Parser, 2), NewText);
string_command(<<"`", Rest/binary>>, Parser, #command{text = C, line = Line}) ->
    Text = #text_to_process{text = lists:reverse(C), line = Line},
    {Rest, inc_pos(Parser), #call{name = <<"shell_exec">>,
                                  args = [Text], line = Line}};
string_command(<<"\\n", Rest/binary>>, Parser, #command{text = T} = S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\n>>|R];
        T ->
            [<<$\n>>|T]
    end,
    string_command(Rest, add_pos(Parser, 2), S#command{text = NewT});
string_command(<<"\\t", Rest/binary>>, Parser, #command{text = T} = S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\t>>|R];
        T ->
            [<<$\t>>|T]
    end,
    string_command(Rest, add_pos(Parser, 2), S#command{text = NewT});
string_command(<<"\\r", Rest/binary>>, Parser, #command{text = T} = S) ->
    NewT = case T of
        [C|R] when is_binary(C) ->
            [<<C/binary, $\r>>|R];
        T ->
            [<<$\r>>|T]
    end,
    string_command(Rest, add_pos(Parser, 2), S#command{text = NewT});
string_command(<<"\n", Rest/binary>>, Parser, #command{text = [C|R]} = S)
        when is_binary(C) ->
    NewText = S#command{text = [<<C/binary, "\n">>|R]},
    string_command(Rest, new_line(Parser), NewText);
string_command(<<"${", Rest/binary>>, Parser, #command{text = C} = S) ->
    NewParser = enclosed_level(add_pos(Parser, 2)),
    {<<"}", Rest0/binary>>, Parser0, [Var]} = variable(Rest, NewParser, []),
    NewText = S#command{text = [Var|C]},
    string_command(Rest0, copy_level(Parser, inc_pos(Parser0)), NewText);
string_command(<<"{$",Rest/binary>>, Parser, #command{text = C} = S) ->
    NewParser = enclosed_level(add_pos(Parser, 2)),
    {<<"}", Rest0/binary>>, Parser0, [Var]} = variable(Rest, NewParser, []),
    NewText = S#command{text = [Var|C]},
    string_command(Rest0, inc_pos(copy_level(Parser, Parser0)), NewText);
string_command(<<"$", A:8, Rest/binary>>, Parser,
               #command{text=C}=S) when ?IS_ALPHA(A) orelse A =:= $_ ->
    NewParser = unclosed_level(inc_pos(Parser)),
    {Rest0, Parser0, [Var]} = variable(<<A:8, Rest/binary>>, NewParser, []),
    NewText = S#command{text = [Var|C]},
    string_command(Rest0, copy_level(Parser, Parser0), NewText);
string_command(<<A/utf8, Rest/binary>>, Parser, #command{text = [C|R]} = S)
        when is_binary(C) ->
    NewText = S#command{text = [<<C/binary, A/utf8>>|R]},
    string_command(Rest, inc_pos(Parser), NewText);
string_command(Rest, Parser, #command{text = C} = S)
        when not is_binary(C) ->
    string_command(Rest, Parser, S#command{text = [<<>>|C]}).

-module(ephp_parser_class).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([st_class/3, st_interface/3, class_name/3]).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-import(ephp_parser, [
    add_pos/2, new_line/1, remove_spaces/2, inc_pos/1
]).

st_interface(<<SP:8, Rest/binary>>, Parser, Interface) when ?IS_SPACE(SP) ->
    st_interface(Rest, inc_pos(Parser), Interface);
st_interface(<<SP:8, Rest/binary>>, Parser, Interface) when ?IS_NEWLINE(SP) ->
    st_interface(Rest, new_line(Parser), Interface);
st_interface(<<A:8, Rest/binary>>, Parser, #class{name =  undefined} = I)
        when ?IS_ALPHA(A) orelse A =:= $_ ->
    {Rest0, Parser0, {NS, Name}} =
        class_name(<<A:8, Rest/binary>>, Parser, []),
    st_interface(Rest0, Parser0, I#class{name = Name, namespace = NS});
st_interface(<<E:8,X:8,T:8,E:8,N:8,D:8,S:8,SP:8,Rest/binary>>, Parser,
             Interface) when ?OR(E,$E,$e) andalso ?OR(X,$X,$x) andalso
                             ?OR(T,$T,$t) andalso ?OR(N,$N,$n) andalso
                             ?OR(D,$D,$d) andalso ?OR(S,$S,$s) andalso
                             (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 5)),
    {Rest1, Parser1, {NS, Extends}} = class_name(Rest0, Parser0, []),
    st_interface(Rest1, Parser1, Interface#class{extends = Extends, extends_ns = NS});
st_interface(<<"{", Rest/binary>>, Parser, Interface) ->
    st_interface_content(Rest, normal_public_level(inc_pos(Parser)), Interface).

st_class(<<SP:8,Rest/binary>>, Parser, Class) when ?IS_SPACE(SP) ->
    st_class(Rest, inc_pos(Parser), Class);
st_class(<<SP:8,Rest/binary>>, Parser, Class) when ?IS_NEWLINE(SP) ->
    st_class(Rest, new_line(Parser), Class);
st_class(<<A:8,Rest/binary>>, Parser, #class{name = undefined} = C)
        when ?IS_ALPHA(A) orelse A =:= $_ ->
    {Rest0, Parser0, {NS, Name}} =
        class_name(<<A:8, Rest/binary>>, Parser, []),
    st_class(Rest0, Parser0, C#class{name = Name, namespace = NS});
st_class(<<$\\, _/binary>>, Parser, _Class) ->
    ephp_parser:throw_error(eparse, Parser, {<<"\\">>, <<"T_NS_SEPARATOR">>, <<"{">>});
st_class(<<E:8,X:8,T:8,E:8,N:8,D:8,S:8,SP:8, Rest/binary>>, Parser, Class) when
        ?OR(E,$E,$e) andalso ?OR(X,$X,$x) andalso ?OR(T,$T,$t) andalso
        ?OR(N,$N,$n) andalso ?OR(D,$D,$d) andalso ?OR(S,$S,$s) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 5)),
    {Rest1, Parser1, {NS, Extends}} = class_name(Rest0, Parser0, []),
    st_class(Rest1, Parser1, Class#class{extends = Extends, extends_ns = NS});
st_class(<<I:8,M:8,P:8,L:8,E:8,M:8,E:8,N:8,T:8,S:8, SP:8, Rest/binary>>,
         Parser, Class) when
        ?OR(I,$I,$i) andalso ?OR(M,$M,$m) andalso ?OR(P,$P,$p) andalso
        ?OR(L,$L,$l) andalso ?OR(E,$E,$e) andalso ?OR(N,$N,$n) andalso
        ?OR(T,$T,$t) andalso ?OR(S,$S,$s) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0} = remove_spaces(<<SP:8, Rest/binary>>, add_pos(Parser, 5)),
    {Rest1, Parser1, Implements} = st_implements(Rest0, Parser0, []),
    st_class(Rest1, Parser1, Class#class{implements = Implements});
st_class(<<"{",Rest/binary>>, Parser, Class) ->
    st_class_content(Rest, normal_public_level(inc_pos(Parser)), Class).

st_interface_content(<<SP:8, Rest/binary>>, Parser, Interface) when ?IS_SPACE(SP) ->
    st_interface_content(Rest, inc_pos(Parser), Interface);
st_interface_content(<<SP:8, Rest/binary>>, Parser, Interface) when ?IS_NEWLINE(SP) ->
    st_interface_content(Rest, new_line(Parser), Interface);
st_interface_content(<<"}", Rest/binary>>, Parser, Class) ->
    {Rest, inc_pos(Parser), Class};
st_interface_content(<<";", Rest/binary>>, Parser, Interface) ->
    st_interface_content(Rest, normal_public_level(inc_pos(Parser)), Interface);
st_interface_content(<<"//", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = ephp_parser:comment_line(Rest, Parser, Parsed),
    st_interface_content(Rest0, Parser0, Parsed);
st_interface_content(<<"#", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = ephp_parser:comment_line(Rest, Parser, Parsed),
    st_interface_content(Rest0, Parser0, Parsed);
st_interface_content(<<"/*", Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = ephp_parser:comment_block(Rest, Parser, Parsed),
    st_interface_content(Rest0, Parser0, Parsed);
st_interface_content(<<P:8,U:8,B:8,L:8,I:8,C:8, SP:8, Rest/binary>>, Parser, Interface) when
        ?OR(P,$P,$p) andalso ?OR(U,$U,$u) andalso ?OR(B,$B,$b) andalso
        ?OR(L,$L,$l) andalso ?OR(I,$I,$i) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_interface_content(<<SP:8, Rest/binary>>, public_level(add_pos(Parser, 6)), Interface);
st_interface_content(<<P:8,R:8,I:8,V:8,A:8,T:8,E:8, SP:8, Rest/binary>>,
                     Parser, Interface) when
        ?OR(P,$P,$p) andalso ?OR(R,$R,$r) andalso ?OR(I,$I,$i) andalso
        ?OR(V,$V,$v) andalso ?OR(A,$A,$a) andalso ?OR(T,$T,$t) andalso
        ?OR(E,$E,$e) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_interface_content(<<SP:8, Rest/binary>>, private_level(add_pos(Parser, 7)),
                         Interface);
st_interface_content(<<P:8,R:8,O:8,T:8,E:8,C:8,T:8,E:8,D:8,SP:8,Rest/binary>>,
                     Parser, Interface) when
        ?OR(P,$P,$p) andalso ?OR(R,$R,$r) andalso ?OR(O,$O,$o) andalso
        ?OR(T,$T,$t) andalso ?OR(E,$E,$e) andalso ?OR(C,$C,$c) andalso
        ?OR(D,$D,$d) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_interface_content(<<SP:8, Rest/binary>>, protected_level(add_pos(Parser, 9)),
                         Interface);
st_interface_content(<<S:8,T:8,A:8,T:8,I:8,C:8,SP:8,Rest/binary>>,
                     Parser, Class) when
        ?OR(S,$S,$s) andalso ?OR(T,$T,$t) andalso ?OR(A,$A,$a) andalso
        ?OR(I,$I,$i) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_interface_content(<<SP:8,Rest/binary>>, static_level(add_pos(Parser,6)),
                         Class);
st_interface_content(<<F:8,U:8,N:8,C:8,T:8,I:8,O:8,N:8,SP:8,Rest/binary>>,
                     #parser{level = Type, access = RawAccess, final = Final} = Parser,
                     #class{methods = Methods} = Interface) when
        ?OR(F,$F,$f) andalso ?OR(U,$U,$u) andalso ?OR(N,$N,$n) andalso
        ?OR(C,$C,$c) andalso ?OR(T,$T,$t) andalso ?OR(I,$I,$i) andalso
        ?OR(O,$O,$o) andalso ?IS_SPACE(SP) ->
    {Rest0, Parser0, [#function{} = Fun]} =
        ephp_parser_func:st_function(Rest, add_pos(Parser, 9), []),
    % TODO: if code is defined it should to throw an error or only ignore it?
    Method = #class_method{name = Fun#function.name,
                           args = Fun#function.args,
                           type = Type,
                           access = access(RawAccess),
                           final = Final},
    NewInterface = Interface#class{methods = Methods ++ [Method]},
    st_interface_content(Rest0, normal_public_level(Parser0), NewInterface);
st_interface_content(<<C:8,O:8,N:8,S:8,T:8, SP:8, Rest/binary>>,
                     Parser, #class{constants = Constants} = Interface) when
        ?OR(C,$c,$C) andalso ?OR(O,$o,$O) andalso ?OR(N,$n,$N) andalso
        ?OR(S,$s,$S) andalso ?OR(T,$t,$T) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    % FIXME assign when a left-constant should be converted in a simple constant
    {Rest0, Parser0, #assign{variable = #constant{} = Cons, expression = Value}} =
        ephp_parser_expr:expression(<<SP:8,Rest/binary>>, add_pos(Parser, 5), []),
    Constant = #class_const{name = Cons#constant.name,
                            value = Value},
    NewInterface = Interface#class{constants = Constants ++ [Constant]},
    st_interface_content(Rest0, normal_public_level(Parser0), NewInterface).

st_class_content(<<SP:8,Rest/binary>>, Parser, Class) when ?IS_SPACE(SP) ->
    st_class_content(Rest, inc_pos(Parser), Class);
st_class_content(<<SP:8,Rest/binary>>, Parser, Class) when ?IS_NEWLINE(SP) ->
    st_class_content(Rest, new_line(Parser), Class);
st_class_content(<<"}",Rest/binary>>, Parser, Class) ->
    NewParser = inc_pos(Parser),
    {Rest, NewParser, ephp_parser:add_line(Class, NewParser)};
st_class_content(<<";",Rest/binary>>, Parser, Class) ->
    st_class_content(Rest, normal_public_level(inc_pos(Parser)), Class);
st_class_content(<<"//",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = ephp_parser:comment_line(Rest, Parser, Parsed),
    st_class_content(Rest0, Parser0, Parsed);
st_class_content(<<"#",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = ephp_parser:comment_line(Rest, Parser, Parsed),
    st_class_content(Rest0, Parser0, Parsed);
st_class_content(<<"/*",Rest/binary>>, Parser, Parsed) ->
    {Rest0, Parser0, _} = ephp_parser:comment_block(Rest, Parser, Parsed),
    st_class_content(Rest0, Parser0, Parsed);
st_class_content(<<P:8,U:8,B:8,L:8,I:8,C:8,SP:8,Rest/binary>>, Parser, Class) when
        ?OR(P,$P,$p) andalso ?OR(U,$U,$u) andalso ?OR(B,$B,$b) andalso
        ?OR(L,$L,$l) andalso ?OR(I,$I,$i) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, public_level(add_pos(Parser, 6)), Class);
st_class_content(<<P:8,R:8,I:8,V:8,A:8,T:8,E:8,SP:8,Rest/binary>>,
                 Parser, Class) when
        ?OR(P,$P,$p) andalso ?OR(R,$R,$r) andalso ?OR(I,$I,$i) andalso
        ?OR(V,$V,$v) andalso ?OR(A,$A,$a) andalso ?OR(T,$T,$t) andalso
        ?OR(E,$E,$e) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, private_level(add_pos(Parser, 7)),
                     Class);
st_class_content(<<P:8,R:8,O:8,T:8,E:8,C:8,T:8,E:8,D:8,SP:8,Rest/binary>>,
                 Parser, Class) when
        ?OR(P,$P,$p) andalso ?OR(R,$R,$r) andalso ?OR(O,$O,$o) andalso
        ?OR(T,$T,$t) andalso ?OR(E,$E,$e) andalso ?OR(C,$C,$c) andalso
        ?OR(D,$D,$d) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, protected_level(add_pos(Parser, 9)),
                     Class);
st_class_content(<<F:8,I:8,N:8,A:8,L:8,SP:8,Rest/binary>>, Parser, Class) when
        ?OR(F,$F,$f) andalso ?OR(I,$I,$i) andalso ?OR(N,$N,$n) andalso
        ?OR(A,$A,$a) andalso ?OR(L,$L,$l) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, final_level(add_pos(Parser, 5)),
                     Class);
st_class_content(<<S:8,T:8,A:8,T:8,I:8,C:8,SP:8,Rest/binary>>,
                 Parser, Class) when
        ?OR(S,$S,$s) andalso ?OR(T,$T,$t) andalso ?OR(A,$A,$a) andalso
        ?OR(I,$I,$i) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, static_level(add_pos(Parser, 6)),
                     Class);
st_class_content(<<V:8,A:8,R:8,SP:8,Rest/binary>>, Parser, Class) when
        ?OR(V,$V,$v) andalso ?OR(A,$A,$a) andalso ?OR(R,$R,$r) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, public_level(add_pos(Parser, 3)), Class);
st_class_content(<<"$",_/binary>> = Rest,
                 #parser{level = Type, access = Access, final = Final} = Parser,
                 #class{attrs = Attrs} = Class) when Access =/= undefined orelse
                                                     Type =:= static ->
    Attr = case ephp_parser_expr:expression(Rest, Parser, []) of
        {Rest0, Parser0, #assign{variable = #variable{name = VarName},
                                 expression = Expr}} ->
            #class_attr{access = access(Access),
                        name = VarName,
                        type = Type,
                        init_value = Expr,
                        final = Final,
                        class_name = Class#class.name,
                        namespace = Class#class.namespace};
        {Rest0, Parser0, #variable{name = VarName}} ->
            #class_attr{access = access(Access),
                        name = VarName,
                        type = Type,
                        final = Final,
                        class_name = Class#class.name,
                        namespace = Class#class.namespace}
    end,
    NewClass = Class#class{attrs = Attrs ++ [Attr]},
    st_class_content(Rest0, normal_public_level(Parser0), NewClass);
st_class_content(<<"$", _/binary>> = _Rest, Parser, _Class) ->
    ephp_parser:throw_error(eparse, Parser, {<<"`\"function (T_FUNCTION)\"'">>});
st_class_content(<<F:8,U:8,N:8,C:8,T:8,I:8,O:8,N:8,SP:8,Rest/binary>>,
                 #parser{level = Type, access = RawAccess, final = Final} = Parser,
                 #class{methods = Methods} = Class) when
        ?OR(F,$F,$f) andalso ?OR(U,$U,$u) andalso ?OR(N,$N,$n) andalso
        ?OR(C,$C,$c) andalso ?OR(T,$T,$t) andalso ?OR(I,$I,$i) andalso
        ?OR(O,$O,$o) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Parser0, [#function{} = Fun]} =
        ephp_parser_func:st_function(Rest, add_pos(Parser, 9), []),
    Method = #class_method{name = Fun#function.name,
                           args = Fun#function.args,
                           code = Fun#function.code,
                           type = Type,
                           access = access(RawAccess),
                           final = Final},
    NewClass = Class#class{methods = Methods ++ [Method]},
    st_class_content(Rest0, normal_public_level(Parser0), NewClass);
st_class_content(<<C:8,O:8,N:8,S:8,T:8, SP:8, Rest/binary>>,
                 Parser, #class{constants = Constants} = Class) when
        ?OR(C,$c,$C) andalso ?OR(O,$o,$O) andalso ?OR(N,$n,$N) andalso
        ?OR(S,$s,$S) andalso ?OR(T,$t,$T) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    % FIXME assign when a left-constant should be converted in a simple constant
    {Rest0, Parser0, #assign{variable = #constant{} = Cons, expression = Value}} =
        ephp_parser_expr:expression(<<SP:8, Rest/binary>>, add_pos(Parser, 5), []),
    Constant = #class_const{name = Cons#constant.name, value = Value},
    NewClass = Class#class{constants = Constants ++ [Constant]},
    st_class_content(Rest0, normal_public_level(Parser0), NewClass);
st_class_content(<<A:8,B:8,S:8,T:8,R:8,A:8,C:8,T:8, SP:8, Rest/binary>>,
                 Parser, Class) when
        ?OR(A,$A,$a) andalso ?OR(B,$B,$b) andalso ?OR(S,$S,$s) andalso
        ?OR(T,$T,$t) andalso ?OR(R,$R,$r) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    NewParser = abstract_level(add_pos(Parser, 8)),
    st_class_content(<<SP:8, Rest/binary>>, NewParser, Class).

access(undefined) -> public;
access(Other) -> Other.

get_ns({[<<>>], Name}, _Parser) -> {[], Name};
get_ns({NS, Name}, Parser) ->
    RawNS = ephp_parser:get_ns(NS ++ [Name], Parser),
    ephp_ns:split(RawNS).

class_name(<<A:8,Rest/binary>>, Parser, []) when ?IS_ALPHA(A) orelse A =:= $_ orelse A =:= $\\ ->
    class_name(Rest, inc_pos(Parser), [<<A:8>>]);
class_name(<<A:8,Rest/binary>>, Parser, [N])
        when ?IS_ALPHA(A) orelse ?IS_NUMBER(A) orelse A =:= $_ orelse A =:= $\\ ->
    class_name(Rest, inc_pos(Parser), [<<N/binary, A:8>>]);
class_name(Rest, Parser, [Parsed]) ->
    ProcessedParsed = get_ns(ephp_ns:parse(Parsed), Parser),
    {Rest, Parser, ProcessedParsed}.

st_implements(<<SP:8, Rest/binary>>, Parser, Parsed) when ?IS_SPACE(SP) ->
    st_implements(Rest, inc_pos(Parser), Parsed);
st_implements(<<SP:8, Rest/binary>>, Parser, Parsed) when ?IS_NEWLINE(SP) ->
    st_implements(Rest, new_line(Parser), Parsed);
st_implements(<<",", Rest/binary>>, Parser, Parsed) ->
    st_implements(Rest, inc_pos(Parser), Parsed);
st_implements(<<A:8, _/binary>> = Rest, Parser, Parsed) when ?IS_ALPHA(A) ->
    {Rest0, Parser0, {NS, Name}} = class_name(Rest, Parser, []),
    st_implements(Rest0, Parser0, [{NS, Name}|Parsed]);
st_implements(<<"{", _/binary>> = Rest, Parser, Parsed) ->
    {Rest, Parser, Parsed}.

normal_public_level(Parser) -> Parser#parser{level = normal, final = false, access = undefined}.
public_level(Parser) -> Parser#parser{access = public}.
protected_level(Parser) -> Parser#parser{access = protected}.
private_level(Parser) -> Parser#parser{access = private}.
static_level(Parser) -> Parser#parser{level = static}.
abstract_level(Parser) -> Parser#parser{level = abstract}.
final_level(Parser) -> Parser#parser{final = true}.

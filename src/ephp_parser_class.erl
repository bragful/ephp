-module(ephp_parser_class).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([st_class/3, st_interface/3]).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-import(ephp_parser, [
    add_pos/2, new_line/1, remove_spaces/2
]).

st_interface(<<SP:8,Rest/binary>>, Pos, Interface) when ?IS_SPACE(SP) ->
    st_interface(Rest, add_pos(Pos, 1), Interface);
st_interface(<<SP:8,Rest/binary>>, Pos, Interface) when ?IS_NEWLINE(SP) ->
    st_interface(Rest, new_line(Pos), Interface);
st_interface(<<A:8, Rest/binary>>, Pos, #interface{name=undefined}=I)
        when ?IS_ALPHA(A) ->
    {Rest0, Pos0, Name} =
        ephp_parser_func:funct_name(<<A:8, Rest/binary>>, Pos, []),
    st_interface(Rest0, Pos0, I#interface{name=Name});
st_interface(<<I:8,M:8,P:8,L:8,E:8,M:8,E:8,N:8,T:8,S:8,SP:8,Rest/binary>>,
             Pos, Interface) when
        ?OR(I,$I,$i) andalso ?OR(M,$M,$m) andalso ?OR(P,$P,$p) andalso
        ?OR(L,$L,$l) andalso ?OR(E,$E,$e) andalso ?OR(N,$N,$n) andalso
        ?OR(T,$T,$t) andalso ?OR(S,$S,$s) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Pos0} = remove_spaces(<<SP:8,Rest/binary>>, add_pos(Pos,5)),
    {Rest1, Pos1, Implements} = st_implements(Rest0, Pos0, []),
    st_interface(Rest1, Pos1, Interface#interface{implements=Implements});
st_interface(<<"{",Rest/binary>>, Pos, Interface) ->
    st_interface_content(Rest, normal_public_level(add_pos(Pos, 1)), Interface).

st_class(<<SP:8,Rest/binary>>, Pos, Class) when ?IS_SPACE(SP) ->
    st_class(Rest, add_pos(Pos,1), Class);
st_class(<<SP:8,Rest/binary>>, Pos, Class) when ?IS_NEWLINE(SP) ->
    st_class(Rest, new_line(Pos), Class);
st_class(<<A:8,Rest/binary>>, Pos, #class{name=undefined}=C)
        when ?IS_ALPHA(A) ->
    {Rest0, Pos0, Name} =
        ephp_parser_func:funct_name(<<A:8,Rest/binary>>, Pos, []),
    st_class(Rest0, Pos0, C#class{name=Name});
st_class(<<E:8,X:8,T:8,E:8,N:8,D:8,S:8,SP:8,Rest/binary>>, Pos, Class) when
        ?OR(E,$E,$e) andalso ?OR(X,$X,$x) andalso ?OR(T,$T,$t) andalso
        ?OR(N,$N,$n) andalso ?OR(D,$D,$d) andalso ?OR(S,$S,$s) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Pos0} = remove_spaces(<<SP:8,Rest/binary>>, add_pos(Pos,5)),
    {Rest1, Pos1, Extends} = ephp_parser_func:funct_name(Rest0, Pos0, []),
    st_class(Rest1, Pos1, Class#class{extends=Extends});
st_class(<<I:8,M:8,P:8,L:8,E:8,M:8,E:8,N:8,T:8,S:8,SP:8,Rest/binary>>,
         Pos, Class) when
        ?OR(I,$I,$i) andalso ?OR(M,$M,$m) andalso ?OR(P,$P,$p) andalso
        ?OR(L,$L,$l) andalso ?OR(E,$E,$e) andalso ?OR(N,$N,$n) andalso
        ?OR(T,$T,$t) andalso ?OR(S,$S,$s) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    {Rest0, Pos0} = remove_spaces(<<SP:8,Rest/binary>>, add_pos(Pos,5)),
    {Rest1, Pos1, Implements} = st_implements(Rest0, Pos0, []),
    st_class(Rest1, Pos1, Class#class{implements=Implements});
st_class(<<"{",Rest/binary>>, Pos, Class) ->
    st_class_content(Rest, normal_public_level(add_pos(Pos,1)), Class).

st_interface_content(<<SP:8, Rest/binary>>, Pos, Interface) when ?IS_SPACE(SP) ->
    st_interface_content(Rest, add_pos(Pos, 1), Interface);
st_interface_content(<<SP:8, Rest/binary>>, Pos, Interface) when ?IS_NEWLINE(SP) ->
    st_interface_content(Rest, new_line(Pos), Interface);
st_interface_content(<<"}", Rest/binary>>, Pos, Class) ->
    {Rest, add_pos(Pos, 1), Class};
st_interface_content(<<";", Rest/binary>>, Pos, Interface) ->
    st_interface_content(Rest, normal_public_level(add_pos(Pos, 1)), Interface);
st_interface_content(<<P:8,U:8,B:8,L:8,I:8,C:8,SP:8,Rest/binary>>, Pos, Interface) when
        ?OR(P,$P,$p) andalso ?OR(U,$U,$u) andalso ?OR(B,$B,$b) andalso
        ?OR(L,$L,$l) andalso ?OR(I,$I,$i) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_interface_content(<<SP:8, Rest/binary>>, public_level(add_pos(Pos, 6)), Interface);
st_interface_content(<<P:8,R:8,I:8,V:8,A:8,T:8,E:8,SP:8,Rest/binary>>,
                     Pos, Interface) when
        ?OR(P,$P,$p) andalso ?OR(R,$R,$r) andalso ?OR(I,$I,$i) andalso
        ?OR(V,$V,$v) andalso ?OR(A,$A,$a) andalso ?OR(T,$T,$t) andalso
        ?OR(E,$E,$e) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_interface_content(<<SP:8, Rest/binary>>, private_level(add_pos(Pos, 7)),
                         Interface);
st_interface_content(<<P:8,R:8,O:8,T:8,E:8,C:8,T:8,E:8,D:8,SP:8,Rest/binary>>,
                     Pos, Interface) when
        ?OR(P,$P,$p) andalso ?OR(R,$R,$r) andalso ?OR(O,$O,$o) andalso
        ?OR(T,$T,$t) andalso ?OR(E,$E,$e) andalso ?OR(C,$C,$c) andalso
        ?OR(D,$D,$d) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_interface_content(<<SP:8, Rest/binary>>, protected_level(add_pos(Pos, 9)),
                         Interface);
st_interface_content(<<F:8,U:8,N:8,C:8,T:8,I:8,O:8,N:8,SP:8,Rest/binary>>,
                     {{RawAccess, Type}, _, _} = Pos,
                     #interface{methods = Methods} = Interface) when
        ?OR(F,$F,$f) andalso ?OR(U,$U,$u) andalso ?OR(N,$N,$n) andalso
        ?OR(C,$C,$c) andalso ?OR(T,$T,$t) andalso ?OR(I,$I,$i) andalso
        ?OR(O,$O,$o) andalso ?IS_SPACE(SP) ->
    {Rest0, Pos0, [#function{}=Fun]} =
        ephp_parser_func:st_function(Rest, add_pos(Pos, 9), []),
    % TODO: if code is defined it should to throw an error or only ignore it?
    Method = #class_method{
        name = Fun#function.name,
        args = Fun#function.args,
        type = Type,
        access = access(RawAccess)},
    NewInterface = Interface#interface{methods=Methods ++ [Method]},
    st_interface_content(Rest0, normal_public_level(Pos0), NewInterface);
st_interface_content(<<C:8,O:8,N:8,S:8,T:8,SP:8,Rest/binary>>,
                     Pos, #interface{constants=Constants} = Interface) when
        ?OR(C,$c,$C) andalso ?OR(O,$o,$O) andalso ?OR(N,$n,$N) andalso
        ?OR(S,$s,$S) andalso ?OR(T,$t,$T) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    % FIXME assign when a left-constant should be converted in a simple constant
    {Rest0, Pos0, #assign{variable=#constant{}=Cons, expression=Value}} =
        ephp_parser_expr:expression(<<SP:8,Rest/binary>>, add_pos(Pos,5), []),
    Constant = #class_const{name = Cons#constant.name,
                            value = Value},
    NewInterface = Interface#interface{constants = Constants ++ [Constant]},
    st_interface_content(Rest0, normal_public_level(Pos0), NewInterface).

st_class_content(<<SP:8,Rest/binary>>, Pos, Class) when ?IS_SPACE(SP) ->
    st_class_content(Rest, add_pos(Pos,1), Class);
st_class_content(<<SP:8,Rest/binary>>, Pos, Class) when ?IS_NEWLINE(SP) ->
    st_class_content(Rest, new_line(Pos), Class);
st_class_content(<<"}",Rest/binary>>, Pos, Class) ->
    {Rest, add_pos(Pos,1), Class};
st_class_content(<<";",Rest/binary>>, Pos, Class) ->
    st_class_content(Rest, normal_public_level(add_pos(Pos, 1)), Class);
st_class_content(<<P:8,U:8,B:8,L:8,I:8,C:8,SP:8,Rest/binary>>, Pos, Class) when
        ?OR(P,$P,$p) andalso ?OR(U,$U,$u) andalso ?OR(B,$B,$b) andalso
        ?OR(L,$L,$l) andalso ?OR(I,$I,$i) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, public_level(add_pos(Pos,6)), Class);
st_class_content(<<P:8,R:8,I:8,V:8,A:8,T:8,E:8,SP:8,Rest/binary>>,
                 Pos, Class) when
        ?OR(P,$P,$p) andalso ?OR(R,$R,$r) andalso ?OR(I,$I,$i) andalso
        ?OR(V,$V,$v) andalso ?OR(A,$A,$a) andalso ?OR(T,$T,$t) andalso
        ?OR(E,$E,$e) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, private_level(add_pos(Pos,7)),
                     Class);
st_class_content(<<P:8,R:8,O:8,T:8,E:8,C:8,T:8,E:8,D:8,SP:8,Rest/binary>>,
                 Pos, Class) when
        ?OR(P,$P,$p) andalso ?OR(R,$R,$r) andalso ?OR(O,$O,$o) andalso
        ?OR(T,$T,$t) andalso ?OR(E,$E,$e) andalso ?OR(C,$C,$c) andalso
        ?OR(D,$D,$d) andalso (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, protected_level(add_pos(Pos,9)),
                     Class);
st_class_content(<<S:8,T:8,A:8,T:8,I:8,C:8,SP:8,Rest/binary>>,
                 Pos, Class) when
        ?OR(S,$S,$s) andalso ?OR(T,$T,$t) andalso ?OR(A,$A,$a) andalso
        ?OR(I,$I,$i) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, static_level(add_pos(Pos,6)),
                     Class);
st_class_content(<<V:8,A:8,R:8,SP:8,Rest/binary>>, Pos, Class) when
        ?OR(V,$V,$v) andalso ?OR(A,$A,$a) andalso ?OR(R,$R,$r) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    st_class_content(<<SP:8,Rest/binary>>, public_level(add_pos(Pos,3)), Class);
st_class_content(<<"$",_/binary>> = Rest, {{Access,Type},_,_}=Pos,
                 #class{attrs=Attrs}=Class) when Access =/= unset ->
    Attr = case ephp_parser_expr:expression(Rest, Pos, []) of
        {Rest0, Pos0, #assign{variable=#variable{name=VarName},
                              expression=Expr}} ->
            #class_attr{
                access = Access,
                name = VarName,
                type = Type,
                init_value = Expr};
        {Rest0, Pos0, #variable{name = VarName}} ->
            #class_attr{
                access = Access,
                name = VarName,
                type = Type}
    end,
    NewClass = Class#class{attrs=Attrs ++ [Attr]},
    st_class_content(Rest0, normal_public_level(Pos0), NewClass);
st_class_content(<<"$",_/binary>> = _Rest, Pos, _Class) ->
    ephp_parser:throw_error(eparse, Pos, {<<"function (T_FUNCTION)">>});
st_class_content(<<F:8,U:8,N:8,C:8,T:8,I:8,O:8,N:8,SP:8,Rest/binary>>,
                 {{RawAccess,Type},_,_}=Pos, #class{methods=Methods}=Class) when
        ?OR(F,$F,$f) andalso ?OR(U,$U,$u) andalso ?OR(N,$N,$n) andalso
        ?OR(C,$C,$c) andalso ?OR(T,$T,$t) andalso ?OR(I,$I,$i) andalso
        ?OR(O,$O,$o) andalso ?IS_SPACE(SP) ->
    {Rest0, Pos0, [#function{}=Fun]} =
        ephp_parser_func:st_function(Rest, add_pos(Pos,9), []),
    Method = #class_method{
        name = Fun#function.name,
        args = Fun#function.args,
        code = Fun#function.code,
        type = Type,
        access = access(RawAccess)},
    NewClass = Class#class{methods=Methods ++ [Method]},
    st_class_content(Rest0, normal_public_level(Pos0), NewClass);
st_class_content(<<C:8,O:8,N:8,S:8,T:8,SP:8,Rest/binary>>,
                 Pos, #class{constants=Constants}=Class) when
        ?OR(C,$c,$C) andalso ?OR(O,$o,$O) andalso ?OR(N,$n,$N) andalso
        ?OR(S,$s,$S) andalso ?OR(T,$t,$T) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    % FIXME assign when a left-constant should be converted in a simple constant
    {Rest0, Pos0, #assign{variable=#constant{}=Cons, expression=Value}} =
        ephp_parser_expr:expression(<<SP:8,Rest/binary>>, add_pos(Pos,5), []),
    Constant = #class_const{
        name=Cons#constant.name,
        value=Value},
    NewClass = Class#class{constants=Constants ++ [Constant]},
    st_class_content(Rest0, normal_public_level(Pos0), NewClass);
st_class_content(<<A:8,B:8,S:8,T:8,R:8,A:8,C:8,T:8,SP:8,Rest/binary>>,
                 Pos, Class) when
        ?OR(A,$A,$a) andalso ?OR(B,$B,$b) andalso ?OR(S,$S,$s) andalso
        ?OR(T,$T,$t) andalso ?OR(R,$R,$r) andalso ?OR(C,$C,$c) andalso
        (?IS_SPACE(SP) orelse ?IS_NEWLINE(SP)) ->
    NewPos = abstract_level(add_pos(Pos,8)),
    st_class_content(<<SP:8,Rest/binary>>, NewPos, Class).

access(unset) -> public;
access(Other) -> Other.

st_implements(<<SP:8,Rest/binary>>, Pos, Parsed) when ?IS_SPACE(SP) ->
    st_implements(Rest, add_pos(Pos,1), Parsed);
st_implements(<<SP:8,Rest/binary>>, Pos, Parsed) when ?IS_NEWLINE(SP) ->
    st_implements(Rest, new_line(Pos), Parsed);
st_implements(<<",",Rest/binary>>, Pos, Parsed) ->
    st_implements(Rest, add_pos(Pos,1), Parsed);
st_implements(<<A:8,_/binary>> = Rest, Pos, Parsed) when ?IS_ALPHA(A) ->
    {Rest0, Pos0, Name} = ephp_parser_func:funct_name(Rest, Pos, []),
    st_implements(Rest0, Pos0, [Name|Parsed]);
st_implements(<<"{",_/binary>> = Rest, Pos, Parsed) ->
    {Rest, Pos, Parsed}.

normal_public_level({_,Row,Col}) -> {{unset,normal},Row,Col}.
public_level({{_,Type},Row,Col}) -> {{public,Type},Row,Col}.
protected_level({{_,Type},Row,Col}) -> {{protected,Type},Row,Col}.
private_level({{_,Type},Row,Col}) -> {{private,Type},Row,Col}.
static_level({{Access,_},Row,Col}) -> {{Access,static},Row,Col}.
abstract_level({{Access,_},Row,Col}) -> {{Access,abstract},Row,Col}.

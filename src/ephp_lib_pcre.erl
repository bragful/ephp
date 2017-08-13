-module(ephp_lib_pcre).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-behaviour(ephp_func).

-export([
    init_func/0,
    init_config/0,
    init_const/0,

    preg_match/7,
    preg_replace/7
]).

-define(PREG_OFFSET_CAPTURE, 256).

-include("ephp.hrl").
-include("ephp_parser.hrl").

-spec init_func() -> ephp_func:php_function_results().

init_func() -> [
    {preg_match, [{args, {2, 5, false, [string,
                                        string,
                                        {raw, ephp_array:new()},
                                        {integer, ?PREG_OFFSET_CAPTURE},
                                        {integer, 0}]}}]},
    {preg_replace, [{args, {3, 5, undefined, [mixed, mixed, mixed,
                                              {integer, -1},
                                              {integer, 0}]}}]}
].

-spec init_config() -> ephp_func:php_config_results().

init_config() -> [].

-spec init_const() -> ephp_func:php_const_results().

init_const() ->
    [
        {<<"PREG_PATTERN_ORDER">>, 1},
        {<<"PREG_SET_ORDER">>, 2},
        {<<"PREG_OFFSET_CAPTURE">>, ?PREG_OFFSET_CAPTURE},
        {<<"PREG_SPLIT_NO_EMPTY">>, 1},
        {<<"PREG_SPLIT_DELIM_CAPTURE">>, 2},
        {<<"PREG_SPLIT_OFFSET_CAPTURE">>, 4},
        {<<"PREG_NO_ERROR">>, 0},
        {<<"PREG_INTERNAL_ERROR">>, 1},
        {<<"PREG_BACKTRACK_LIMIT_ERROR">>, 2},
        {<<"PREG_RECURSION_LIMIT_ERROR">>, 3},
        {<<"PREG_BAD_UTF8_ERROR">>, 4},
        {<<"PREG_BAD_UTF8_OFFSET_ERROR">>, 5},
        {<<"PCRE_VERSION">>, <<?PCRE_VERSION>>}
    ].

-spec preg_replace(context(), line(),
                   Pattern :: var_value(),
                   Replacement :: var_value(),
                   Subject :: var_value(),
                   Limit :: var_value(),
                   Count :: var_value()) -> binary() | ephp_array() | undefined.

%% TODO: check when Pattern, Replacement and Subject are arrays
preg_replace(_Context, _Line, {_, Pattern}, {_, Replacement}, {_, Subject},
                              {_, Limit}, {undefined, _}) ->
    {RegExp, Flags} = get_parts(Pattern),
    PMFlags = case Limit of
        -1 -> Flags;
        _ -> [{match_limit, Limit}|Flags]
    end,
    Replace = parse_replace(Replacement),
    re:replace(Subject, RegExp, Replace, [{return, binary}|PMFlags]).

-spec preg_match(
    context(), line(),
    Pattern :: var_value(),
    Subject :: var_value(),
    Matches :: var_value(),
    Flags :: var_value(),
    Offset :: var_value()) -> pos_integer().

preg_match(_Context, _Line, {_,Pattern}, {_,Subject}, {undefined, _},
           {_, _}, {_, Offset}) ->
    {RegExp, Flags} = get_parts(Pattern),
    PMFlags = [{offset, Offset}],
    case re:run(Subject, RegExp, Flags ++ PMFlags) of
        {match, _} -> 1;
        nomatch -> 0
    end;

preg_match(Context, _Line, {_,Pattern}, {_,Subject}, {VarMatches,_},
           {_, Flag}, {_, Offset}) ->
    {RegExp, Flags} = get_parts(Pattern),
    PMFlags = [{capture, first, index}, {offset, Offset}],
    ValMatches = case re:run(Subject, RegExp, Flags ++ PMFlags) of
        {match, [{I,S}]} ->
            <<_:I/binary,R:S/binary,_/binary>> = Subject,
            if
                Flag =:= ?PREG_OFFSET_CAPTURE ->
                    ephp_array:from_list([ephp_array:from_list([R, I])]);
                true ->
                    ephp_array:from_list([R])
            end;
        nomatch ->
            ephp_array:new()
    end,
    ephp_context:set(Context, VarMatches, ValMatches),
    ephp_array:size(ValMatches).

%%------------------------------------------------------------------------------
%% Internal functions

get_parts(<<InitDelim:8,Rest/binary>>) ->
    EndDelim = get_end_delimiter(InitDelim),
    {RegExp, RawFlags} = parse_regexp(Rest, EndDelim),
    Flags = parse_flags(RawFlags),
    {RegExp, Flags}.

parse_replace(Replacement) ->
    parse_replace(Replacement, <<>>).

parse_replace(<<>>, Result) ->
    Result;
parse_replace(<<"\\",Rest/binary>>, Result) ->
    parse_replace(Rest, <<Result/binary, "\\\\">>);
parse_replace(<<"${",Rest/binary>>, Result) ->
    case binary:split(Rest, <<"}">>) of
        [Num, NewRest] ->
            parse_replace(NewRest, <<Result/binary,"\\",Num/binary>>);
        _ ->
            %% TODO: error?
            ok
    end;
parse_replace(<<"$",Rest/binary>>, Result) ->
    {NewRest, Num} = parse_number(Rest, <<>>),
    parse_replace(NewRest, <<Result/binary,"\\",Num/binary>>);
parse_replace(<<A/utf8,Rest/binary>>, Result) ->
    parse_replace(Rest, <<Result/binary,A/utf8>>).

parse_number(<<A:8,Rest/binary>>, Result) when A >= 0 andalso A =< 9 ->
    parse_number(Rest, <<Result/binary, A:8>>);
parse_number(Rest, Result) ->
    {Rest, Result}.

parse_regexp(String, EndDelim) ->
    parse_regexp(String, <<>>, EndDelim).

parse_regexp(<<>>, _Result, _EndDelim) ->
    throw({error, enodelim});
parse_regexp(<<EndDelim:8,Rest/binary>>, Result, EndDelim) ->
    {Result, Rest};
parse_regexp(<<"(",Rest/binary>>, Result, EndDelim) ->
    {NewResult, NewRest} = parse_regexp(Rest, $)),
    parse_regexp(NewRest, <<Result/binary,"(",NewResult/binary,")">>, EndDelim);
parse_regexp(<<"[",Rest/binary>>, Result, EndDelim) ->
    {NewResult, NewRest} = parse_regexp(Rest, $]),
    parse_regexp(NewRest, <<Result/binary,"[",NewResult/binary,"]">>, EndDelim);
parse_regexp(<<"<",Rest/binary>>, Result, EndDelim) ->
    {NewResult, NewRest} = parse_regexp(Rest, $>),
    parse_regexp(NewRest, <<Result/binary,"<",NewResult/binary,">">>, EndDelim);
parse_regexp(<<"{",Rest/binary>>, Result, EndDelim) ->
    {NewResult, NewRest} = parse_regexp(Rest, $}),
    parse_regexp(NewRest, <<Result/binary,"{",NewResult/binary,"}">>, EndDelim);
parse_regexp(<<A:8, Rest/binary>>, Result, EndDelim) ->
    parse_regexp(Rest, <<Result/binary, A:8>>, EndDelim).

parse_flags(RawFlags) ->
    parse_flags(RawFlags, []).

parse_flags(<<"i",RestFlags/binary>>, Flags) ->
    parse_flags(RestFlags, [caseless|Flags]);
parse_flags(<<"m",RestFlags/binary>>, Flags) ->
    parse_flags(RestFlags, [multiline|Flags]);
parse_flags(<<"s",RestFlags/binary>>, Flags) ->
    parse_flags(RestFlags, [dotall|Flags]);
parse_flags(<<"x",RestFlags/binary>>, Flags) ->
    parse_flags(RestFlags, [extended|Flags]);
parse_flags(<<"A",RestFlags/binary>>, Flags) ->
    parse_flags(RestFlags, [anchored|Flags]);
parse_flags(<<"D",RestFlags/binary>>, Flags) ->
    parse_flags(RestFlags, [dollar_endonly|Flags]);
parse_flags(<<"U",RestFlags/binary>>, Flags) ->
    parse_flags(RestFlags, [ungreedy|Flags]);
parse_flags(<<"J",RestFlags/binary>>, Flags) ->
    parse_flags(RestFlags, [dupnames|Flags]);
parse_flags(<<"u",RestFlags/binary>>, Flags) ->
    parse_flags(RestFlags, [unicode,bsr_unicode|Flags]);
parse_flags(<<"X",RestFlags/binary>>, Flags) ->
    %% TODO
    parse_flags(RestFlags, Flags);
parse_flags(<<"S",RestFlags/binary>>, Flags) ->
    %% TODO
    parse_flags(RestFlags, Flags);
parse_flags(<<>>, Flags) ->
    Flags.

get_end_delimiter($() -> $);
get_end_delimiter($<) -> $>;
get_end_delimiter($[) -> $];
get_end_delimiter(${) -> $};
get_end_delimiter($\\) -> throw({error, edelimiter});
get_end_delimiter(A) when ?IS_ALPHA(A) -> throw({error, edelimiter});
get_end_delimiter(A) when ?IS_NUMBER(A) -> throw({error, edelimiter});
get_end_delimiter(A) when ?IS_SPACE(A) -> throw({error, edelimiter});
get_end_delimiter(A) -> A.

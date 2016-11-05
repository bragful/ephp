-module(ephp_parser_test).
-author('manuel@altenwald.com').
-compile([warnings_as_errors,export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("ephp.hrl").

-define(PARSE, ephp_parser:parse).

only_text_test_() -> [
    ?_assertMatch(
        [#print_text{text = <<"<html><body><h1>Hi world!</h1></body></html>">>}],
        ?PARSE("<html><body><h1>Hi world!</h1></body></html>")),
    ?_assertMatch(
        [#print_text{text = <<"hello world!">>}],
        ?PARSE("hello world!"))
].

different_tags_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#assign{variable=#variable{name = <<"a">>}, expression=#int{int=5}}]}],
        ?PARSE("<? $a = 5; ?>")),
    ?_assertMatch(
        [#print_text{text = <<"5">>}],
        ?PARSE("<?=5?>")),
    ?_assertMatch(
        [#eval{statements=[#assign{variable=#variable{name = <<"a">>}, expression=#int{int=5}}]}],
        ?PARSE("<?php $a = 5; ?>"))
].

php_endtag_optional_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#assign{variable=#variable{name = <<"a">>}, expression=#int{int=5}}]}],
        ?PARSE("<? $a = 5;")),
    ?_assertMatch(
        [#eval{statements=[#assign{variable=#variable{name = <<"a">>}, expression=#int{int=5}}]}],
        ?PARSE("<?php $a = 5;")),
    ?_assertMatch(
        {error, eparse,{{line,1},{column,6}}, ?E_PARSE, <<"...">>},
        catch ?PARSE("<?=5 ")),
    ?_assertMatch(
        [#print_text{text = <<"5 Resultado">>}],
        ?PARSE("<?=5?> Resultado"))
].

literal_test_() -> [
    ?_assertMatch(
        [#print_text{text = <<"-5">>}],
        ?PARSE("<?=-5?>")),
    ?_assertMatch(
        [#print_text{text = <<"5.0">>}],
        ?PARSE("<?=5.0?>")),
    ?_assertMatch(
        [#print_text{text = <<"-5.0">>}],
        ?PARSE("<?= - 5.0 ?>")),
    ?_assertMatch(
        [#print_text{text = <<"hello world!">>}],
        ?PARSE("<?='hello world!'?>")),
    ?_assertMatch(
        [#print_text{text = <<"hello world!">>}],
        ?PARSE("<?=\"hello world!\"?>"))
].

concat_test_() -> [
    ?_assertMatch(
        [#print{expression=#concat{texts=[#text{text = <<"hello ">>},
                 #variable{name = <<"name">>},
                 #text{text = <<".">>},
                 #variable{name = <<"surname">>}]}}],
        ?PARSE("<?=\"hello \" . $name . \".\" . $surname ?>")),
    ?_assertMatch(
        [#print{expression=#concat{texts=[#text{text = <<"hello ">>},
                 #variable{name = <<"name">>},
                 #text{text = <<", welcome!">>}]}}],
        ?PARSE("<?='hello ' . $name . ', welcome!'?>")),
    ?_assertMatch(
        [#print_text{text = <<"hello world!">>}],
        ?PARSE("<?='hello ' . 'world!'?>"))
].

arith_test_() -> [
    ?_assertMatch(
        [#print_text{text = <<"126">>}],
        ?PARSE("<?= 1 + 25 * 5 ?>")),
    ?_assertMatch(
        [#print_text{text = <<"26">>}],
        ?PARSE("<?=1+25?>")),
    ?_assertMatch(
        [#print_text{text = <<"130">>}],
        ?PARSE("<?= (1 + 25) * 5 ?>"))
].

multi_statement_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#assign{variable=#variable{name = <<"a">>},expression=#int{int=1}},
        #assign{variable=#variable{name = <<"b">>}, expression=#int{int=2}},
        #assign{variable=#variable{name = <<"c">>},
                expression=
                #operation{type = <<"+">>,
                           expression_left = #variable{name = <<"a">>},
                           expression_right = #operation{
                                type = <<"*">>,
                                expression_left=#variable{name = <<"b">>},
                                expression_right=#int{int=25}}}}]}],
        ?PARSE("<?php $a = 1; $b = 2; $c = $a + $b * 25; ?>"))
].

if_statement_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#if_block{
            conditions=#operation{type = <<">">>,
                expression_left = #variable{name = <<"a">>},
                expression_right = #int{int=5}},
            true_block=[#assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}}]}]}],
        ?PARSE("<?php if ($a > 5) $a = 0; ?>")),
    ?_assertMatch(
        [#eval{statements=[#if_block{
            conditions=#operation{type = <<">">>,
                expression_left = #variable{name = <<"a">>},
                expression_right = #int{int=5}},
            true_block = [#assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}}],
            false_block = [#assign{variable=#variable{name = <<"a">>}, expression=#int{int=5}}]}]}],
        ?PARSE("<?php if ($a > 5) $a = 0; else $a = 5; ?>")),
    ?_assertMatch(
        [#eval{statements=[#if_block{
            conditions=#operation{type = <<">">>,
                expression_left = #variable{name = <<"a">>},
                expression_right = #int{int=5}},
            true_block = [#assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}}],
            false_block = [#if_block{
                conditions=#operation{type = <<">">>,
                    expression_left = #variable{name = <<"a">>},
                    expression_right = #int{int=3}},
                true_block = [#assign{variable=#variable{name = <<"a">>}, expression=#int{int=5}}]}]}]}],
        ?PARSE("<?php if ($a > 5) $a = 0; elseif ($a > 3) $a = 5; ?>"))
].

if_statement_codeblock_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#if_block{
            conditions=#operation{type = <<">">>,
                expression_left = #variable{name = <<"a">>},
                expression_right = #int{int=5}},
            true_block = [#assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}}]}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; } ?>")),
    ?_assertMatch(
        [#eval{statements=[#if_block{
            conditions=#operation{type = <<">">>,
                expression_left = #variable{name = <<"a">>},
                expression_right = #int{int=5}},
            true_block = [#assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}}],
            false_block = [#assign{variable=#variable{name = <<"a">>}, expression=#int{int=5}}]}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; } else { $a = 5; } ?>"))
].

if_statement_literalblock_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#if_block{
            conditions = #operation{type = <<">">>,
                expression_left = #variable{name = <<"a">>},
                expression_right = #int{int=5}},
            true_block = [
                #assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}},
                #print_text{text = <<" OK ">>}]}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; ?> OK <? } ?>")),
    ?_assertMatch(
        [#eval{statements=[#if_block{
            conditions = #operation{type = <<">">>,
                expression_left = #variable{name = <<"a">>},
                expression_right = #int{int=5}},
            true_block = [
                #assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}},
                #print_text{text = <<" OK ">>},
                #print{expression=#variable{name = <<"name">>}},
                #print_text{text = <<"! ">>}]}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; ?> OK <?=$name?>! <? } ?>"))
].

ternary_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#assign{variable=#variable{name = <<"a">>},
                expression=#if_block{
                    conditions=#operation{type = <<">">>,
                        expression_left=#variable{name = <<"a">>},
                        expression_right=#int{int=5}},
                    true_block=#int{int=0},
                    false_block=#int{int=5}}}]}],
        ?PARSE("<?php $a = ($a > 5) ? 0 : 5; ?>"))
].

var_indexes_test_() -> [
    ?_assertMatch(
        [#print{expression=#variable{name = <<"a">>, idx=[#int{int=25},#variable{name = <<"b">>}]}}],
        ?PARSE("<?=$a[25][$b]?>")),
    ?_assertMatch(
        [#print{expression=#variable{name = <<"a">>,
             idx=[#operation{type = <<"+">>,
                expression_left=#int{int=25},
                expression_right=#variable{name = <<"i">>}},
              #variable{name = <<"b">>}]}}],
        ?PARSE("<?=$a[25+$i][$b]?>"))
].

while_statement_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#while{
            type=pre,
            conditions=#operation{type = <<">">>,
                expression_left=#variable{name = <<"a">>},
                expression_right=#int{int=5}},
            loop_block=[#assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}}]}]}],
        ?PARSE("<?php while ($a > 5) $a = 0; ?>"))
].

while_statement_codeblock_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#while{
            type=pre,
            conditions=#operation{type = <<">">>,
                expression_left=#variable{name = <<"a">>},
                expression_right=#int{int=5}},
            loop_block=[#assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}}]}]}],
        ?PARSE("<?php while ($a > 5) { $a = 0; } ?>"))
].

while_statement_literalblock_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#while{
            type=pre,
            conditions=#operation{type = <<">">>,
                expression_left=#variable{name = <<"a">>},
                expression_right=#int{int=5}},
            loop_block=[
                #assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}},
                #print_text{text = <<" OK ">>}]}]}],
        ?PARSE("<?php while ($a > 5) { $a = 0; ?> OK <? } ?>")),
    ?_assertMatch(
        [#eval{statements=[#while{
            type=pre,
            conditions=#operation{type = <<">">>,
                expression_left=#variable{name = <<"a">>},
                expression_right=#int{int=5}},
            loop_block=[
                #assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}},
                #print_text{text = <<" OK ">>},
                #print{expression=#variable{name = <<"name">>}},
                #print_text{text = <<"! ">>}]}]}],
        ?PARSE("<?php while ($a > 5) { $a = 0; ?> OK <?=$name?>! <? } ?>"))
].

do_while_statement_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#while{
            type=post,
            conditions=#operation{type = <<">">>,
                expression_left=#variable{name = <<"a">>},
                expression_right=#int{int=5}},
            loop_block=[#assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}}]}]}],
        ?PARSE("<?php do $a = 0; while ($a > 5); ?>"))
].

do_while_statement_codeblock_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#while{
            type=post,
            conditions=#operation{type = <<">">>,
                expression_left=#variable{name = <<"a">>},
                expression_right=#int{int=5}},
            loop_block=[#assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}}]}]}],
        ?PARSE("<?php do { $a = 0; } while ($a > 5); ?>"))
].

do_while_statement_literalblock_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#while{
            type=post,
            conditions=#operation{type = <<">">>,
                expression_left=#variable{name = <<"a">>},
                expression_right=#int{int=5}},
            loop_block=[
                #assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}},
                #print_text{text = <<" OK ">>}]}]}],
        ?PARSE("<?php do { $a = 0; ?> OK <? } while ($a > 5); ?>")),
    ?_assertMatch(
        [#eval{statements=[#while{
            type=post,
            conditions=#operation{type = <<">">>,
                expression_left=#variable{name = <<"a">>},
                expression_right=#int{int=5}},
            loop_block=[
                #assign{variable=#variable{name = <<"a">>}, expression=#int{int=0}},
                #print_text{text = <<" OK ">>},
                #print{expression=#variable{name = <<"name">>}},
                #print_text{text = <<"! ">>}]}]}],
        ?PARSE("<?php do { $a = 0; ?> OK <?=$name?>! <? } while ($a > 5); ?>"))
].

for_statement_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#for{
            init=[#assign{variable=#variable{name = <<"i">>}, expression=#int{int=0}}],
            conditions=#operation{type = <<"<">>,
                expression_left = #variable{name = <<"i">>},
                expression_right = #int{int=5}},
            update=[#assign{variable=#variable{name = <<"i">>}, expression=
                #operation{type = <<"+">>,
                    expression_left = #variable{name = <<"i">>},
                    expression_right = #int{int=1}}}],
            loop_block=[#assign{variable=#variable{name = <<"b">>},
                expression = #operation{type = <<"+">>,
                    expression_left = #variable{name = <<"b">>},
                    expression_right = #variable{name = <<"i">>}}}]}]}],
        ?PARSE("<?php for ($i=0;$i<5;$i=$i+1) $b = $b + $i; ?>")),
    ?_assertMatch([#eval{statements = [#for{
        init = [#assign{variable=#variable{name = <<"i">>}, expression=#int{int=0}}],
        conditions = #operation{
            type = <<"<">>,
            expression_left = #variable{name = <<"i">>},
            expression_right = #int{int=5}},
        update = [{post_incr, #variable{name = <<"i">>}, _}],
        loop_block = [#assign{variable = #variable{name = <<"b">>}, expression =
            #operation{type = <<"+">>,
                expression_left = #variable{name = <<"b">>},
                expression_right = #variable{name = <<"i">>}}}]}]}],
        ?PARSE("<?php for ($i=0;$i<5;$i++) $b = $b + $i;"))
].

foreach_statement_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#foreach{
                 iter=#variable{name = <<"i">>},
                 elements=#variable{name = <<"data">>},
                 loop_block=#assign{variable=#variable{name = <<"b">>},
                         expression=#operation{
                            type = <<"+">>,
                            expression_left=#variable{name = <<"b">>},
                            expression_right=#variable{name = <<"i">>}}}}]}],
        ?PARSE("<?php foreach ($data as $i) $b = $b + $i; ?>")),
    ?_assertMatch(
        [#eval{statements=[#foreach{
                 kiter=#variable{name = <<"k">>},
                 iter=#variable{name = <<"i">>},
                 elements=#variable{name = <<"data">>},
                 loop_block=#assign{variable=#variable{name = <<"b">>},
                        expression=#operation{type = <<"+">>,
                            expression_left=#variable{name = <<"b">>},
                            expression_right=#variable{name = <<"i">>}}}}]}],
        ?PARSE("<?php foreach ($data as $k => $i) $b = $b + $i; ?>"))
].

switch_statement_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#switch{condition=#variable{name = <<"a">>},
                cases=[#switch_case{label=#text{text = <<"hola">>},
                    code_block=[#assign{variable=#variable{name = <<"a">>},
                        expression=#int{int=5}}]}]}]}],
        ?PARSE("<?php switch ($a) { case 'hola': $a = 5; } ?>")),
    ?_assertMatch(
        [#eval{statements=[#switch{
            condition=#variable{name = <<"a">>},
            cases=[#switch_case{label=#text{text = <<"hola">>},
                code_block=[
                    #assign{variable=#variable{name = <<"a">>},
                        expression=#int{int=5}},
                    break]}]}]}],
        ?PARSE("<?php switch ($a) { case 'hola': $a = 5; break; } ?>")),
    ?_assertMatch(
        [#eval{statements=[#switch{
            condition=#variable{name = <<"a">>},
            cases=[
                #switch_case{label=#text{text = <<"hola">>},
                    code_block=[
                        #assign{variable=#variable{name = <<"a">>},
                            expression=#int{int=5}},
                        break]},
                #switch_case{label=default,
                    code_block=[
                        #assign{variable=#variable{name = <<"a">>},
                            expression=#int{int=0}}]}]}]}],
        ?PARSE("<?php switch ($a) { case 'hola': $a = 5; break; default: $a = 0; } ?>"))
].

post_incr_test_() -> [
    ?_assertMatch(
        [#eval{statements=[{post_incr,#variable{name = <<"i">>},_}]}],
        ?PARSE("<? $i++; ")),
    ?_assertMatch(
        [#print{expression={post_incr,#variable{name = <<"i">>}, _}}],
        ?PARSE("<?=$i++?>"))
].

post_decr_test_() -> [
    ?_assertMatch(
        [#eval{statements=[{post_decr,#variable{name = <<"i">>}, _}]}],
        ?PARSE("<? $i--; ")),
    ?_assertMatch(
        [#print{expression={post_decr,#variable{name = <<"i">>}, _}}],
        ?PARSE("<?=$i--?>"))
].

pre_incr_test_() -> [
    ?_assertMatch(
        [#eval{statements=[{pre_incr,#variable{name = <<"i">>}, _}]}],
        ?PARSE("<? ++$i; ")),
    ?_assertMatch(
        [#print{expression={pre_incr,#variable{name = <<"i">>}, _}}],
        ?PARSE("<?=++$i?>"))
].

pre_decr_test_() -> [
    ?_assertMatch(
        [#eval{statements=[{pre_decr,#variable{name = <<"i">>}, _}]}],
        ?PARSE("<? --$i; ")),
    ?_assertMatch(
        [#print{expression={pre_decr,#variable{name = <<"i">>}, _}}],
        ?PARSE("<?=--$i?>"))
].

pre_and_post_incr_and_decr_as_index_test_() -> [
    ?_assertMatch(
        [#print{expression = #variable{name = <<"a">>, idx=[{post_incr,#variable{name = <<"i">>}, _}]}}],
        ?PARSE("<?=$a[$i++]?>")),
    ?_assertMatch(
        [#print{expression = #variable{name = <<"a">>, idx=[{pre_decr,#variable{name = <<"i">>}, _}]}}],
        ?PARSE("<?=$a[--$i]?>")),
    ?_assertMatch(
        [#print{expression = #variable{name = <<"a">>, idx=[{post_decr,#variable{name = <<"i">>}, _}]}}],
        ?PARSE("<?=$a[$i--]?>")),
    ?_assertMatch(
        [#print{expression = #variable{name = <<"a">>, idx=[{pre_incr,#variable{name = <<"i">>}, _}]}}],
        ?PARSE("<?=$a[++$i]?>"))
].

not_test_() -> [
    ?_assertMatch(
        [#print{expression=#if_block{
            conditions={operation_not,#variable{name = <<"a">>},_},
            true_block=#int{int=1}}}],
        ?PARSE("<?=!$a ? 1?>")),
    ?_assertMatch(
        [#eval{statements=[#if_block{
            conditions={operation_not,#variable{name = <<"a">>},_},
            true_block=[#assign{variable=#variable{name = <<"a">>}, expression=#int{int=5}}]}]}],
        ?PARSE("<? if (!$a) $a = 5; ?>")),
    ?_assertMatch(
        [#eval{statements=[#assign{variable=#variable{name = <<"a">>},
            expression={operation_not,#variable{name = <<"a">>}, _}}]}],
        ?PARSE("<? $a = !$a; ?>")),
    ?_assertMatch(
        [#print{expression={operation_not,#variable{name = <<"a">>}, _}}],
        ?PARSE("<?=!$a?>"))
].

function_call_test_() -> [
    ?_assertMatch(
        [#eval{statements=[#assign{variable=#variable{name = <<"a">>},
            expression=#call{name = <<"myfunc">>, args=[#int{int=1},#int{int=2},#int{int=3}]}}]}],
        ?PARSE("<? $a = myfunc( 1, 2, 3 ); ")),
    ?_assertMatch(
        [#print{expression=#call{name = <<"myfunc">>}}],
        ?PARSE("<?=myfunc()?>"))
].

comments_test_() -> [
    ?_assertMatch(
        [],
        ?PARSE("<? # comment ?>")),
    ?_assertMatch(
        [],
        ?PARSE("<? // comment ")),
    ?_assertMatch(
        [],
        ?PARSE("<? /* comment */ ")),
    ?_assertMatch(
        [#eval{statements=[#print_text{text = <<"Result = ">>},
            #print{expression = #variable{name = <<"i">>}}]}],
        ?PARSE("<? # comentando ?>Result = <?=$i?><? // another comment more..."))
].

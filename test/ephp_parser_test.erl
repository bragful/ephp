-module(ephp_parser_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-define(PARSE, ephp_parser:parse).

only_text_test_() -> [
    ?_assertEqual(
        [{print_text,<<"<html><body><h1>Hi world!</h1></body></html>">>}],
        ?PARSE("<html><body><h1>Hi world!</h1></body></html>")),
    ?_assertEqual(
        [{print_text,<<"hello world!">>}],
        ?PARSE("hello world!"))
].

different_tags_test_() -> [
    ?_assertEqual(
        [{eval,[{assign,{variable,<<"a">>,[]},{int,5}}]}],
        ?PARSE("<? $a = 5; ?>")),
    ?_assertEqual(
        [{print,{int,5}}],
        ?PARSE("<?=5?>")),
    ?_assertEqual(
        [{eval,[{assign,{variable,<<"a">>,[]},{int,5}}]}],
        ?PARSE("<?php $a = 5; ?>"))
].

php_endtag_optional_test_() -> [
    ?_assertEqual(
        [{eval,[{assign,{variable,<<"a">>,[]},{int,5}}]}],
        ?PARSE("<? $a = 5;")),
    ?_assertEqual(
        [{eval,[{assign,{variable,<<"a">>,[]},{int,5}}]}],
        ?PARSE("<?php $a = 5;")),
    ?_assertEqual(
        {[],<<"<?=5 ">>,{{line,1},{column,1}}},
        ?PARSE("<?=5 ")),
    ?_assertEqual(
        [{print,{int,5}},{print_text,<<" Resultado">>}],
        ?PARSE("<?=5?> Resultado"))
].

literal_test_() -> [
    ?_assertEqual(
        [{print,{int,5}}],
        ?PARSE("<?=5?>")),
    ?_assertEqual(
        [{print,{int,-5}}],
        ?PARSE("<?=-5?>")),
    ?_assertEqual(
        [{print,{float,5.0}}],
        ?PARSE("<?=5.0?>")),
    ?_assertEqual(
        [{print,{float,-5.0}}],
        ?PARSE("<?= - 5.0 ?>")),
    ?_assertEqual(
        [{print,{text,<<"hello world!">>}}],
        ?PARSE("<?='hello world!'?>")),
    ?_assertEqual(
        [{print,{text,<<"hello world!">>}}],
        ?PARSE("<?=\"hello world!\"?>"))
].

concat_test_() -> [
    ?_assertEqual(
        [{print,{concat,[{text,<<"hello ">>},
                 {variable,<<"name">>,[]},
                 {text,<<".">>},
                 {variable,<<"surname">>,[]}]}}],
        ?PARSE("<?=\"hello \" . $name . \".\" . $surname ?>")),
    ?_assertEqual(
        [{print,{concat,[{text,<<"hello ">>},
                 {variable,<<"name">>,[]},
                 {text,<<", welcome!">>}]}}],
        ?PARSE("<?='hello ' . $name . ', welcome!'?>")),
    ?_assertEqual(
        [{print,{text,<<"hello world!">>}}],
        ?PARSE("<?='hello ' . 'world!'?>"))
].

arith_test_() -> [
    ?_assertEqual(
        [{print,{int,126}}],
        ?PARSE("<?= 1 + 25 * 5 ?>")),
    ?_assertEqual(
        [{print,{int,26}}],
        ?PARSE("<?=1+25?>")),
    ?_assertEqual(
        [{print,{int,130}}],
        ?PARSE("<?= (1 + 25) * 5 ?>"))
].

multi_statement_test_() -> [
    ?_assertEqual(
        [{eval,[{assign,{variable,<<"a">>,[]},{int,1}},
        {assign,{variable,<<"b">>,[]},{int,2}},
        {assign,{variable,<<"c">>,[]},
                {operation,<<"+">>,
                           {variable,<<"a">>,[]},
                           {operation,<<"*">>,{variable,<<"b">>,[]},{int,25}}}}]}],
        ?PARSE("<?php $a = 1; $b = 2; $c = $a + $b * 25; ?>"))
].

if_statement_test_() -> [
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  {assign,{variable,<<"a">>,[]},{int,0}}, undefined}]}],
        ?PARSE("<?php if ($a > 5) $a = 0; ?>")),
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  {assign,{variable,<<"a">>,[]},{int,0}},
                  {assign,{variable,<<"a">>,[]},{int,5}}}]}],
        ?PARSE("<?php if ($a > 5) $a = 0; else $a = 5; ?>")),
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  {assign,{variable,<<"a">>,[]},{int,0}},
                  {if_block,{operation,<<">">>,{variable,<<"a">>,[]},{int,3}},
                    {assign,{variable,<<"a">>,[]},{int,5}}, undefined}}]}],
        ?PARSE("<?php if ($a > 5) $a = 0; elseif ($a > 3) $a = 5; ?>"))
].

if_statement_codeblock_test_() -> [
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  [{assign,{variable,<<"a">>,[]},{int,0}}], undefined}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; } ?>")),
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  [{assign,{variable,<<"a">>,[]},{int,0}}],
                  [{assign,{variable,<<"a">>,[]},{int,5}}]}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; } else { $a = 5; } ?>"))
].

if_statement_literalblock_test_() -> [
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  [{assign,{variable,<<"a">>,[]},{int,0}},
                   {print_text,<<" OK ">>}], undefined}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; ?> OK <? } ?>")),
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  [{assign,{variable,<<"a">>,[]},{int,0}},
                   {print_text,<<" OK ">>},
                   {print,{variable,<<"name">>,[]}},
                   {print_text,<<"! ">>}], undefined}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; ?> OK <?=$name?>! <? } ?>"))
].

ternary_test_() -> [
    ?_assertEqual(
        [{eval,[{assign,{variable,<<"a">>,[]},
                {if_block,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                          {int,0},
                          {int,5}}}]}],
        ?PARSE("<?php $a = ($a > 5) ? 0 : 5; ?>"))
].

var_indexes_test_() -> [
    ?_assertEqual(
        [{print,{variable,<<"a">>,[{int,25},{variable,<<"b">>,[]}]}}],
        ?PARSE("<?=$a[25][$b]?>")),
    ?_assertEqual(
        [{print,{variable,<<"a">>,
             [{operation,<<"+">>,{int,25},{variable,<<"i">>,[]}},
              {variable,<<"b">>,[]}]}}],
        ?PARSE("<?=$a[25+$i][$b]?>"))
].

while_statement_test_() -> [
    ?_assertEqual(
        [{eval,[{while,pre,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  {assign,{variable,<<"a">>,[]},{int,0}}}]}],
        ?PARSE("<?php while ($a > 5) $a = 0; ?>"))
].

while_statement_codeblock_test_() -> [
    ?_assertEqual(
        [{eval,[{while,pre,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  [{assign,{variable,<<"a">>,[]},{int,0}}]}]}],
        ?PARSE("<?php while ($a > 5) { $a = 0; } ?>"))
].

while_statement_literalblock_test_() -> [
    ?_assertEqual(
        [{eval,[{while,pre,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  [{assign,{variable,<<"a">>,[]},{int,0}},
                   {print_text,<<" OK ">>}]}]}],
        ?PARSE("<?php while ($a > 5) { $a = 0; ?> OK <? } ?>")),
    ?_assertEqual(
        [{eval,[{while,pre,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  [{assign,{variable,<<"a">>,[]},{int,0}},
                   {print_text,<<" OK ">>},
                   {print,{variable,<<"name">>,[]}},
                   {print_text,<<"! ">>}]}]}],
        ?PARSE("<?php while ($a > 5) { $a = 0; ?> OK <?=$name?>! <? } ?>"))
].

do_while_statement_test_() -> [
    ?_assertEqual(
        [{eval,[{while,post,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  {assign,{variable,<<"a">>,[]},{int,0}}}]}],
        ?PARSE("<?php do $a = 0; while ($a > 5); ?>"))
].

do_while_statement_codeblock_test_() -> [
    ?_assertEqual(
        [{eval,[{while,post,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  [{assign,{variable,<<"a">>,[]},{int,0}}]}]}],
        ?PARSE("<?php do { $a = 0; } while ($a > 5); ?>"))
].

do_while_statement_literalblock_test_() -> [
    ?_assertEqual(
        [{eval,[{while,post,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  [{assign,{variable,<<"a">>,[]},{int,0}},
                   {print_text,<<" OK ">>}]}]}],
        ?PARSE("<?php do { $a = 0; ?> OK <? } while ($a > 5); ?>")),
    ?_assertEqual(
        [{eval,[{while,post,{operation,<<">">>,{variable,<<"a">>,[]},{int,5}},
                  [{assign,{variable,<<"a">>,[]},{int,0}},
                   {print_text,<<" OK ">>},
                   {print,{variable,<<"name">>,[]}},
                   {print_text,<<"! ">>}]}]}],
        ?PARSE("<?php do { $a = 0; ?> OK <?=$name?>! <? } while ($a > 5); ?>"))
].

for_statement_test_() -> [
    ?_assertEqual(
        [{eval,[{for,[{assign,{variable,<<"i">>,[]},{int,0}}],
             {operation,<<"<">>,{variable,<<"i">>,[]},{int,5}},
             [{assign,{variable,<<"i">>,[]},
                      {operation,<<"+">>,{variable,<<"i">>,[]},{int,1}}}],
             {assign,{variable,<<"b">>,[]},
                     {operation,<<"+">>,{variable,<<"b">>,[]},{variable,<<"i">>,[]}}}}]}],
        ?PARSE("<?php for ($i=0;$i<5;$i=$i+1) $b = $b + $i; ?>")),
    ?_assertEqual(
        [{eval,[{for,[{assign,{variable,<<"i">>,[]},{int,0}}],
             {operation,<<"<">>,{variable,<<"i">>,[]},{int,5}},
             [{post_incr,{variable,<<"i">>,[]}}],
             {assign,{variable,<<"b">>,[]},
                     {operation,<<"+">>,{variable,<<"b">>,[]},{variable,<<"i">>,[]}}}}]}],
        ?PARSE("<?php for ($i=0;$i<5;$i++) $b = $b + $i;"))
].

foreach_statement_test_() -> [
    ?_assertEqual(
        [{eval,[{foreach,undefined,
                 {variable,<<"i">>,[]},
                 {variable,<<"data">>,[]},
                 {assign,{variable,<<"b">>,[]},
                         {operation,<<"+">>,{variable,<<"b">>,[]},{variable,<<"i">>,[]}}}}]}],
        ?PARSE("<?php foreach ($data as $i) $b = $b + $i; ?>")),
    ?_assertEqual(
        [{eval,[{foreach,
                 {variable,<<"k">>,[]},
                 {variable,<<"i">>,[]},
                 {variable,<<"data">>,[]},
                 {assign,{variable,<<"b">>,[]},
                         {operation,<<"+">>,{variable,<<"b">>,[]},{variable,<<"i">>,[]}}}}]}],
        ?PARSE("<?php foreach ($data as $k => $i) $b = $b + $i; ?>"))
].

switch_statement_test_() -> [
    ?_assertEqual(
        [{eval,[{switch,{variable,<<"a">>,[]},
                [{switch_case,{text,<<"hola">>},
                              [{assign,{variable,<<"a">>,[]},{int,5}}]}]}]}],
        ?PARSE("<?php switch ($a) { case 'hola': $a = 5; } ?>")),
    ?_assertEqual(
        [{eval,[{switch,{variable,<<"a">>,[]},
                [{switch_case,{text,<<"hola">>},
                              [{assign,{variable,<<"a">>,[]},{int,5}},break]}]}]}],
        ?PARSE("<?php switch ($a) { case 'hola': $a = 5; break; } ?>")),
    ?_assertEqual(
        [{eval,[{switch,{variable,<<"a">>,[]},
                [{switch_case,{text,<<"hola">>},
                              [{assign,{variable,<<"a">>,[]},{int,5}},break]},
                 {default,[{assign,{variable,<<"a">>,[]},{int,0}}]}]}]}],
        ?PARSE("<?php switch ($a) { case 'hola': $a = 5; break; default: $a = 0; } ?>"))
].

post_incr_test_() -> [
    ?_assertEqual(
        [{eval,[{post_incr,{variable,<<"i">>,[]}}]}],
        ?PARSE("<? $i++; ")),
    ?_assertEqual(
        [{print,{post_incr,{variable,<<"i">>,[]}}}],
        ?PARSE("<?=$i++?>"))
].

post_decr_test_() -> [
    ?_assertEqual(
        [{eval,[{post_decr,{variable,<<"i">>,[]}}]}],
        ?PARSE("<? $i--; ")),
    ?_assertEqual(
        [{print,{post_decr,{variable,<<"i">>,[]}}}],
        ?PARSE("<?=$i--?>"))
].

pre_incr_test_() -> [
    ?_assertEqual(
        [{eval,[{pre_incr,{variable,<<"i">>,[]}}]}],
        ?PARSE("<? ++$i; ")),
    ?_assertEqual(
        [{print,{pre_incr,{variable,<<"i">>,[]}}}],
        ?PARSE("<?=++$i?>"))
].

pre_decr_test_() -> [
    ?_assertEqual(
        [{eval,[{pre_decr,{variable,<<"i">>,[]}}]}],
        ?PARSE("<? --$i; ")),
    ?_assertEqual(
        [{print,{pre_decr,{variable,<<"i">>,[]}}}],
        ?PARSE("<?=--$i?>"))
].

pre_and_post_incr_and_decr_as_index_test_() -> [
    ?_assertEqual(
        [{print,{variable,<<"a">>,[{post_incr,{variable,<<"i">>,[]}}]}}],
        ?PARSE("<?=$a[$i++]?>")),
    ?_assertEqual(
        [{print,{variable,<<"a">>,[{pre_decr,{variable,<<"i">>,[]}}]}}],
        ?PARSE("<?=$a[--$i]?>")),
    ?_assertEqual(
        [{print,{variable,<<"a">>,[{post_decr,{variable,<<"i">>,[]}}]}}],
        ?PARSE("<?=$a[$i--]?>")),
    ?_assertEqual(
        [{print,{variable,<<"a">>,[{pre_incr,{variable,<<"i">>,[]}}]}}],
        ?PARSE("<?=$a[++$i]?>"))
].

not_test_() -> [
    ?_assertEqual(
        [{print,{if_block,{operation_not,{variable,<<"a">>,[]}},{int,1},undefined}}],
        ?PARSE("<?=!$a ? 1?>")),
    ?_assertEqual(
        [{eval,[{if_block,{operation_not,{variable,<<"a">>,[]}},
                  {assign,{variable,<<"a">>,[]},{int,5}}, undefined}]}],
        ?PARSE("<? if (!$a) $a = 5; ?>")),
    ?_assertEqual(
        [{eval,[{assign,{variable,<<"a">>,[]},
                {operation_not,{variable,<<"a">>,[]}}}]}],
        ?PARSE("<? $a = !$a; ?>")),
    ?_assertEqual(
        [{print,{operation_not,{variable,<<"a">>,[]}}}],
        ?PARSE("<?=!$a?>"))
].

function_call_test_() -> [
    ?_assertEqual(
        [{eval,[{assign,{variable,<<"a">>,[]},
                {call,<<"myfunc">>,[{int,1},{int,2},{int,3}]}}]}],
        ?PARSE("<? $a = myfunc( 1, 2, 3 ); ")),
    ?_assertEqual(
        [{print,{call,<<"myfunc">>,[]}}],
        ?PARSE("<?=myfunc()?>"))
].

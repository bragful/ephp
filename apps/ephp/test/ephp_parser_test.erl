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
        [{eval,[{assign,{var,<<"a">>},{int,5}}]}],
        ?PARSE("<? $a = 5; ?>")),
    ?_assertEqual(
        [{print,{int,5}}],
        ?PARSE("<?=5?>")),
    ?_assertEqual(
        [{eval,[{assign,{var,<<"a">>},{int,5}}]}],
        ?PARSE("<?php $a = 5; ?>"))
].

php_endtag_optional_test_() -> [
    ?_assertEqual(
        [{eval,[{assign,{var,<<"a">>},{int,5}}]}],
        ?PARSE("<? $a = 5;")),
    ?_assertEqual(
        [{eval,[{assign,{var,<<"a">>},{int,5}}]}],
        ?PARSE("<?php $a = 5;")),
    ?_assertEqual(
        {[],<<"<?=5 ">>,{{line,1},{column,1}}},
        ?PARSE("<?=5 "))
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
                 {var,<<"name">>},
                 {text,<<".">>},
                 {var,<<"surname">>}]}}],
        ?PARSE("<?=\"hello \" . $name . \".\" . $surname ?>")),
    ?_assertEqual(
        [{print,{concat,[{text,<<"hello ">>},
                 {var,<<"name">>},
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
        [{eval,[{assign,{var,<<"a">>},{int,1}},
        {assign,{var,<<"b">>},{int,2}},
        {assign,{var,<<"c">>},
                {operation,<<"+">>,
                           {var,<<"a">>},
                           {operation,<<"*">>,{var,<<"b">>},{int,25}}}}]}],
        ?PARSE("<?php $a = 1; $b = 2; $c = $a + $b * 25; ?>"))
].

if_statement_test_() -> [
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  {assign,{var,<<"a">>},{int,0}}}]}],
        ?PARSE("<?php if ($a > 5) $a = 0; ?>")),
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  {assign,{var,<<"a">>},{int,0}},
                  {assign,{var,<<"a">>},{int,5}}}]}],
        ?PARSE("<?php if ($a > 5) $a = 0; else $a = 5; ?>"))
].

if_statement_codeblock_test_() -> [
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  [{assign,{var,<<"a">>},{int,0}}]}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; } ?>")),
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  [{assign,{var,<<"a">>},{int,0}}],
                  [{assign,{var,<<"a">>},{int,5}}]}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; } else { $a = 5; } ?>"))
].

if_statement_literalblock_test_() -> [
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  [{assign,{var,<<"a">>},{int,0}},
                   [{print_text,<<" OK ">>}]]}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; ?> OK <? } ?>")),
    ?_assertEqual(
        [{eval,[{if_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  [{assign,{var,<<"a">>},{int,0}},
                   [{print_text,<<" OK ">>},
                    {print,{var,<<"name">>}},
                    {print_text,<<"! ">>}]]}]}],
        ?PARSE("<?php if ($a > 5) { $a = 0; ?> OK <?=$name?>! <? } ?>"))
].

ternary_test_() -> [
    ?_assertEqual(
        [{eval,[{assign,{var,<<"a">>},
                {if_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                          {return,{int,0}},
                          {return,{int,5}}}}]}],
        ?PARSE("<?php $a = ($a > 5) ? 0 : 5; ?>"))
].

var_indexes_test_() -> [
    ?_assertEqual(
        [{print,{var,<<"a">>,[{int,25},{var,<<"b">>}]}}],
        ?PARSE("<?=$a[25][$b]?>")),
    ?_assertEqual(
        [{print,{var,<<"a">>,
             [{operation,<<"+">>,{int,25},{var,<<"i">>}},
              {var,<<"b">>}]}}],
        ?PARSE("<?=$a[25+$i][$b]?>"))
].

while_statement_test_() -> [
    ?_assertEqual(
        [{eval,[{pre_while_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  {assign,{var,<<"a">>},{int,0}}}]}],
        ?PARSE("<?php while ($a > 5) $a = 0; ?>"))
].

while_statement_codeblock_test_() -> [
    ?_assertEqual(
        [{eval,[{pre_while_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  [{assign,{var,<<"a">>},{int,0}}]}]}],
        ?PARSE("<?php while ($a > 5) { $a = 0; } ?>"))
].

while_statement_literalblock_test_() -> [
    ?_assertEqual(
        [{eval,[{pre_while_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  [{assign,{var,<<"a">>},{int,0}},
                   [{print_text,<<" OK ">>}]]}]}],
        ?PARSE("<?php while ($a > 5) { $a = 0; ?> OK <? } ?>")),
    ?_assertEqual(
        [{eval,[{pre_while_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  [{assign,{var,<<"a">>},{int,0}},
                   [{print_text,<<" OK ">>},
                    {print,{var,<<"name">>}},
                    {print_text,<<"! ">>}]]}]}],
        ?PARSE("<?php while ($a > 5) { $a = 0; ?> OK <?=$name?>! <? } ?>"))
].

do_while_statement_test_() -> [
    ?_assertEqual(
        [{eval,[{post_while_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  {assign,{var,<<"a">>},{int,0}}}]}],
        ?PARSE("<?php do $a = 0; while ($a > 5); ?>"))
].

do_while_statement_codeblock_test_() -> [
    ?_assertEqual(
        [{eval,[{post_while_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  [{assign,{var,<<"a">>},{int,0}}]}]}],
        ?PARSE("<?php do { $a = 0; } while ($a > 5); ?>"))
].

do_while_statement_literalblock_test_() -> [
    ?_assertEqual(
        [{eval,[{post_while_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  [{assign,{var,<<"a">>},{int,0}},
                   [{print_text,<<" OK ">>}]]}]}],
        ?PARSE("<?php do { $a = 0; ?> OK <? } while ($a > 5); ?>")),
    ?_assertEqual(
        [{eval,[{post_while_block,{operation,<<">">>,{var,<<"a">>},{int,5}},
                  [{assign,{var,<<"a">>},{int,0}},
                   [{print_text,<<" OK ">>},
                    {print,{var,<<"name">>}},
                    {print_text,<<"! ">>}]]}]}],
        ?PARSE("<?php do { $a = 0; ?> OK <?=$name?>! <? } while ($a > 5); ?>"))
].

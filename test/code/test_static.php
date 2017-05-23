<?php

static $c = 100;

function greetings() {
    static $a = 10, $b = 20;
    print "($a) ($b) hello world\n";
    $a += 10;
    $b *= 2;
}

greetings();
greetings();
greetings();

<?php

class A {
    public $a = 5;
    public $b = 10;
}

$a = new A();
$a->c = $a->a + $a->b;

if ($a->a < $a->b) print "OK!\n";

print "$a->a + $a->b = $a->c\n";


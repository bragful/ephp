<?php

class A {
    public $sum = 0;
}

$a = new A;
print "sum => $a->sum\n";
++$a->sum;
var_dump($a);
++$a->sum;
var_dump($a);
$a->sum++;
var_dump($a);
$a->sum++;
var_dump($a);


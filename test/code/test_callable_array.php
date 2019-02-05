<?php

class A {
    function b($greet) { print "Says: [$greet] OK\n"; }
}

$a = new A;
$c = [$a, "b"];
$c("master!");

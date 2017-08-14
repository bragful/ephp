<?php

class A {}

$a = new A;
$b = &$a;
$c = &$b;
$d = &$c;

var_dump(["a" => $a, "b" => $b, "c" => $c, "d" => $d]);

unset($a);

var_dump(["a" => $a, "b" => $b, "c" => $c, "d" => $d]);

unset($b);

var_dump(["a" => $a, "b" => $b, "c" => $c, "d" => $d]);

unset($c);

var_dump(["a" => $a, "b" => $b, "c" => $c, "d" => $d]);


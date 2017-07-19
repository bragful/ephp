<?php

class A {}

$a = new A;
$b = $a;
$c = &$a;

print "\$a contents =>\n";
var_dump($a);

unset($a);

print "\n--------------\nnow \$a contents =>\n";
var_dump($a);

print "\n--------------\nand \$b contents =>\n";
var_dump($b);

print "\n--------------\nand \$c contents =>\n";
var_dump($c);

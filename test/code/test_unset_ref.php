<?php

$a = 1;
$b =& $a;

$c = array( &$a, &$b, 1, 2, 3 );
var_dump($c);
unset($c);

var_dump($a);
var_dump($b);
var_dump($c);


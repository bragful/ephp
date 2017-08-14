<?php

$b = 1;
print '$b = ';
var_dump($b);
$a = [ &$b, 2, 3 ];
print '$a = ';
var_dump($a);
print 'we remove $b' . "\n";
unset($b);
print '$a = ';
var_dump($a);
print '$b = ';
var_dump($b);


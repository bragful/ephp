<?php

$a = [ "apple", "orange", "grape", "peach" ];

$i = reset($a);
$current = current($a);
print "[$current]\n";
do {
    print "$i, ";
} while ($i = next($a));
var_dump(next($a));
$current = current($a);
print "\n [$current]\n";

$i = end($a);
$current = current($a);
print "[$current]\n";
do {
    print "$i, ";
} while ($i = prev($a));
var_dump(prev($a));
$current = current($a);
print "\n [$current]\n";

var_dump($a);

$a = array();
var_dump(reset($a));
var_dump(current($a));
var_dump(next($a));
var_dump(prev($a));
var_dump(end($a));

$a = "hey!";
var_dump(reset($a));

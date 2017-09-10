<?php

$a = [ "apple", "orange", "grape", "peach" ];

$i = reset($a);
$current = current($a);
print "[" . key($a) . " => $current]\n";
do {
    print "$i, ";
} while ($i = next($a));
var_dump(next($a));
$current = pos($a);
print "\n [" . key($a) . " => $current]\n";

$i = end($a);
$current = current($a);
print "[" . key($a) . " => $current]\n";
do {
    print "$i, ";
} while ($i = prev($a));
var_dump(prev($a));
$current = pos($a);
print "\n [" . key($a) . " => $current]\n";

var_dump($a);

$a = array();
var_dump(reset($a));
var_dump(current($a));
var_dump(next($a));
var_dump(prev($a));
var_dump(end($a));
var_dump(key($a));

$a = "hey!";
var_dump(reset($a));
var_dump(current($a));
var_dump(key($a));


<?php

$data = [ 1,2,3 ];

print_r($data);
print "is array? ";
print(((is_array($data)) ? "yes" : "no") . "\n");

print "is bool? ";
print(((is_bool($data)) ? "yes" : "no") . "\n");

print "index 1 is integer? ";
print(((is_integer($data[1])) ? "yes" : "no") . "\n");

print "index 0 is numeric? ";
print(((is_numeric($data[0])) ? "yes" : "no") . "\n");

print "index 2 is float? ";
print(((is_float($data[2])) ? "yes" : "no") . "\n");

print "is null? ";
print(((is_null($data)) ? "yes" : "no") . "\n");

print "null is null? ";
print(((is_null(null)) ? "yes" : "no") . "\n");

print "is string? ";
print(((is_string($data)) ? "yes" : "no") . "\n");

print "is object? ";
print(((is_object($data)) ? "yes" : "no") . "\n");

print "element 1 using print_r: ";
print_r($data[1]);
print "\n";

print "element 2 using var_dump: ";
var_dump($data[2]);

print "print_r(2) == 2? ";
print(((print_r(2, true) == 2) ? "yes" : "no") . "\n");

print "isset \$data[5]? ";
print(((isset($data[5])) ? "yes" : "no") . "\n");

print "is empty \$data[0]? ";
print(((empty($data[0])) ? "yes" : "no") . "\n");

print "is empty \$data[5]? ";
print(((empty($data[5])) ? "yes" : "no") . "\n");

print "is empty '0'? ";
print(((empty('0')) ? "yes" : "no") . "\n");

print "is empty ''? ";
print(((empty('')) ? "yes" : "no") . "\n");

print "is empty a false value? ";
print(((empty(false)) ? "yes" : "no") . "\n");


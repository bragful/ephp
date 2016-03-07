<?php

$t = "Hello world!";
$parts = [ "h", "e", "l", "l", "o" ];

print "the string '$t', len=" . strlen($t) . "\n";
print "the parts => " . print_r($parts, true);
print "the parts == " . join(", ", $parts) . "\n";

print "ord('A') = " . ord('A') . "\n";
print "chr(65) = " . chr(65) . "\n";
print "chr('A') = "; var_dump(chr('A'));

print "implode parts = " . implode($parts) . "\n";
print "implode parts = " . implode($parts, [1,2,3,4]) . "\n";
print "implode empty = " . implode([]) . "\n";

print "string to lowercase = " . strtolower("HELLO") . "\n";
print "string to uppercase = " . strtoupper("hello") . "\n";

print "explode word = "; var_dump(explode(",", "h,e,l,l,o"));
print "explode word (limit less than length) = "; var_dump(explode(",", "h,e,l,l,o", 2));
print "explode word (limit equal to length) = "; var_dump(explode(",", "h,e,l,l,o", 5));
print "explode word (limit greater than length) = "; var_dump(explode(",", "h,e,l,l,o", 7));
print "explode word (limit negative less than length) = "; var_dump(explode(",", "h,e,l,l,o", -2));
print "explode word (limit negative equal to length) = "; var_dump(explode(",", "h,e,l,l,o", -4));
print "explode word (limit negative greater than length) = "; var_dump(explode(",", "h,e,l,l,o", -5));
print "explode word (limit 0) = "; var_dump(explode(",", "h,e,l,l,o", 0));


<?php

$t = "Hello world!";
$parts = [ "h", "e", "l", "l", "o" ];

print "the string '$t', len=" . strlen($t) . "\n";
print "the parts => " . print_r($parts, true);
print "the parts == " . join(", ", $parts) . "\n";

print "ord('A') = " . ord('A') . "\n";
print "chr(65) = " . chr(65) . "\n";


<?php

$base = array( 1, 2, 3, 4, 5, array( 8, 9 ) );

print "\$base => " . print_r($base,true);

print "is 1 in array \$base? " . (in_array(1, $base) ? "yes" : "no") . "\n";
print "is 8 in array \$base? " . (in_array(8, $base) ? "yes" : "no") . "\n";
print "is 2 and 3 in array \$base? " . (in_array(array(2,3), $base) ? "yes" : "no") . "\n";
print "is 5 and 8 in array \$base? " . (in_array(array(5,8), $base) ? "yes" : "no") . "\n";
print "is 8 and 9 in array \$base? " . (in_array(array(8,9), $base) ? "yes" : "no") . "\n";
print "how many elementas has \$base? " . count($base) . "\n";
print "what's the size of \$base? " . sizeof($base) . "\n";
print "what's the count for an integer? " . count($base[0]) . "\n";

var_dump(in_array(25,25));

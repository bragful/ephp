<?php

$base = array( 1, 2, 3, 4, 5 );

print "\$base => " . print_r($base,true);

print "is 1 in array \$base? " . (in_array(1, $base) ? "yes" : "no") . "\n";
print "is 8 in array \$base? " . (in_array(8, $base) ? "yes" : "no") . "\n";
print "how many elementas has \$base? " . count($base) . "\n";
print "what's the size of \$base? " . sizeof($base) . "\n";


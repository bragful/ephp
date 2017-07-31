<?php

function fibonacci() {
    static $a = 1;
    static $b = 1;
    $value = $a;
    $c = $a + $b;
    $a = $b;
    $b = $c;
    return $value;
}

print "values: ";
for ($i=0; $i<100; $i++)
    print fibonacci() . ", ";
print "\n";


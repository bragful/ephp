<?php

$a = [1,2,3,4,5,6,7,8,9,10];
$b = ["one", "two"];

foreach ($b as $k) {
    foreach ($a as $i)
        foreach ($a as $j) {
            print "($k) $i x $j = ";
            if ($i * $j > 8) {
                print "greater than 8!\n";
                continue 2;
            }
            print ($i * $j) . "\n";
        }
}

print "OK!\n";

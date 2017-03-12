<?php

$a = [1,2,3,4,5,6,7,8,9,10];
$b = ["one", "two"];

foreach ($b as $k) {
    foreach ($a as $i)
        foreach ($a as $j) {
            if ($i * $j > 8) {
                break 2;
            }
            print "($k) $i x $j = " . ($i * $j) . "\n";
        }
}

print "OK!\n";

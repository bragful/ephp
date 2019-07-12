<?php
$newfunc = create_function('$a,$b', 'return "ln($a) + ln($b) = " . log($a * $b);');
echo "New anonymous function: $newfunc\n";
echo $newfunc(2, M_E) . "\n";

$closure = function($a, $b) { return "ln($a) + ln($b) = " . log($a * $b); };
echo "New closure: $closure\n";
echo $closure(2, M_E) . "\n";

var_dump($newfunc);
var_dump($closure);

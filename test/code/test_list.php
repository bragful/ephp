<?php

$info = array('coffee', 'brown', 'caffeine');

list($drink, $color, $energy) = $info;
echo "The $drink is $color and the $energy makes it special.\n";

list($drink, , $energy) = $info;
echo "The $drink has $energy.\n";

list( , , $energy) = $info;
echo "I need $energy!\n";

list($bar) = "abcde";
var_dump($bar); // NULL

list($a, $b, $c) = [1, 2];
var_dump([$a, $b, $c]);


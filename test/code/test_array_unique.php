<?php

$input = array("a" => "green", "red", "b" => "green", "blue", "red");
$result = array_unique($input, SORT_STRING);
print_r($result);

$input = array(4, "4", "3", 4, 3, "3", "4in", "3cm");
$result = array_unique($input, SORT_NUMERIC);
print_r($result);

$result = array_unique($input, SORT_REGULAR);
print_r($result);

$result = array_unique($input, SORT_STRING);
print_r($result);

print_r(array_unique($input, 10));
print_r(array_unique("this is a string", 10));


<?php

$data = array ( 1, 2, 3 );
$string = "the number is $data[0]\n";
$enclosed = "the number is ${data[1]}\n";

print $string;
print $enclosed;


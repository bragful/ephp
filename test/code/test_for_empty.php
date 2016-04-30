<?php


$a = [];
$b = [1,2,3,4];

for ($i=0; $i<count($b); $a[$i] = $b[$i++])
	;

print_r($a);
print_r($b);


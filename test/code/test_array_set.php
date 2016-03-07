<?php

$a = [];
$a[] = 1;
$a[] = 2;
$a[] = 3;
print_r($a);

for ($i=0; $i<3; $i++) {
	$a[$i] *= $a[$i];
	$a[$a[$i] * 10] = $a[$i];
	print "{$a[$i]} ";
}
print "\n";

foreach ($a as $k => $v) {
	print "$k => $v\n";
}


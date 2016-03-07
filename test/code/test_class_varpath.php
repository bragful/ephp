<?php

class A {
	public $data = [ 1,2,3,4,5 ];
}

$a = new A();
for ($i=0; $i<count($a->data); $i++) {
	$a->data[$i] *= 2;
	print $a->data[$i] . " ";
}
print "\n";


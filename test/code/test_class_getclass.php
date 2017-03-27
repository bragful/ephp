<?php

class A { }

$a = new A;
$classname = "A";

print A::class . "\n";
print "get_class = " . get_class($a) . "\n";

var_dump(get_class(array()));

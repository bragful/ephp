<?php

class A {
    public function __destruct() {
        print "destroying!\n";
    }
}

$a = new A;
$b = new A;
$a->b = $b;

unset($a);
unset($b);

print "END\n";


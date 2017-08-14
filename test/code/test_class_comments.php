<?php

class A { # begin
    // this is an attribute
    public $a = 10;

    /* this is a function */
    public function __toString() { return "$this->a"; }

# end
}

$a = new A;
$a->a = 100;
print $a . "\n";


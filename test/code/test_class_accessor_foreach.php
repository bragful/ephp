<?php

class A {
    public static $a = [ 1, 2, 3 ];
    public $b;

    function __construct() {
        $this->b = [ 1, 2, 3, 4, 5 ];
    }
}

print "static =>\n";
foreach (A::$a as $k => $v) {
    print "$k => $v\n";
}
print "---------\n";

print "dynamic =>\n";
$a = new A;
foreach ($a->b as $k => $v) {
    print "$k -> $v\n";
}
print "----------\n";
foreach ($b->b as $k => $v) {
    print "KO\n";
}

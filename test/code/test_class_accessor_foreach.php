<?php

class A {
    public static $a = [ 1, 2, 3 ];
}

foreach (A::$a as $k => $v) {
    print "$k => $v\n";
}


<?php

class A {
    function b($greet) { print "Says: [$greet] OK\n"; }
}

$hooks = [];
foreach (array ('a', 'b', 'c', 'd') as $filter) {
    if (!isset($hooks[$filter])) {
        $hooks[$filter] = new A;
    }
    $hooks[ $filter ]->b("world $filter");
}

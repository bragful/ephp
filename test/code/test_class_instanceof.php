<?php

class foo {
    function check($data) {
        return $data instanceof self;
    }
}

$a = new foo;
$b = new foo;

// objects are the same
var_dump($a == $b, $a === $b);
var_dump($a instanceof $b);

// classes are the same
var_dump($a instanceof foo);
var_dump($a instanceof bar);

var_dump($b instanceof foo);
var_dump($b instanceof bar);

var_dump($a->check(10), $a->check($b));


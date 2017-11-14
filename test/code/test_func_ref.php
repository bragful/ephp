<?php

function &data() {
    $a = new stdClass;
    $a->hi = "hello world!";
    return $a;
}

$b = data();
var_dump($b);
 

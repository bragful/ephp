<?php

function &data() {
    $a = new stdClass();
    $a->data = 10;
    return $a;
}

$d = &data();
$e = &$d;
var_dump($e);


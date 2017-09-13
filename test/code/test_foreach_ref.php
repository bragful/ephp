<?php

$data = [ "a" => [1], "b" => [2], "c" => [3] ];

foreach($data as &$val) {
    $val[1] = $val[0] * 2;
    $val[2] = $val[0] * 3;
}

var_dump($val);
var_dump($data);


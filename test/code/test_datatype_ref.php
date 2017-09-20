<?php

function data(array &$d) {
    $d['hi'] = 'hello';
}

$trans = [ 'bye' => 'goodbye' ];
data($trans);
var_dump($trans);


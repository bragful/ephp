<?php

class A {
    public $a;
}

$data = [ new A, new A, new A ];
for ($i=0; $i<count($data); $i++)
    $data[$i]->a = 10;

var_dump($data);


<?php

$a = new stdClass;
$a->name = "John";
$a->surname = "Smith";

$b = (array)$a;
$c = (object)$b;

var_dump($a);
var_dump($b);
var_dump($c);

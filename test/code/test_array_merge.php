<?php
$array1    = array("color" => "red", 2, 4);
$array2    = array("a", "b", "color" => "green", "shape" => "trapezoid", 4);
$array3    = array(1, 2, 3, 4);
$resultado = array_merge($array1, $array2);
print_r($resultado);

$comienzo = 'foo';
$fin = array(1 => 'bar');
$resultado = array_merge((array)$comienzo, (array)$fin);
print_r($resultado);

var_dump(array_merge($comienzo, $fin));
var_dump(array_merge([], [], $comienzo, $fin));
var_dump(array_merge($array1, $array2, $array3));


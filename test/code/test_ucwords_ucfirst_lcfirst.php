<?php
$foo = 'hello world!';
$foo = ucwords($foo);             // Hello World!
var_dump($foo);

$bar = 'HELLO WORLD!';
$bar = ucwords($bar);             // HELLO WORLD!
var_dump($bar);
$bar = ucwords(strtolower($bar)); // Hello World!
var_dump($bar);

$foo = 'hello|world!';
$bar = ucwords($foo);             // Hello|world!
var_dump($bar);

$baz = ucwords($foo, "|");        // Hello|World!
var_dump($baz);

$foo = 'hello world!';
$foo = ucfirst($foo);             // Hello world!
var_dump($foo);

$bar = 'HELLO WORLD!';
$bar = ucfirst($bar);             // HELLO WORLD!
var_dump($bar);
$bar = ucfirst(strtolower($bar)); // Hello world!
var_dump($bar);

$foo = 'HelloWorld';
$foo = lcfirst($foo);             // helloWorld
var_dump($foo);

$bar = 'HELLO WORLD!';
$bar = lcfirst($bar);             // hELLO WORLD!
var_dump($bar);
$bar = lcfirst(strtoupper($bar)); // hELLO WORLD!
var_dump($bar);

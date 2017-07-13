<?php

$a = 10;
$b = 20;

$c = [ "GLOBALS" => "nada" ];

var_dump($GLOBALS["c"]);
var_dump($c);

if (isset($GLOBALS)) echo "GLOBALS exists!\n";
if (isset($GLOBALS["c"])) echo "GLOBALS['c'] exists!\n";

$GLOBALS = array( "a" => 10 );
var_dump($GLOBALS);

$GLOBALS["a"] = 11;
var_dump($GLOBALS);

$GLOBALS = 10;
var_dump($GLOBALS);

unset($GLOBALS);
var_dump($GLOBALS);


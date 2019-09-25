<?php

function __autoload($classname) {
    require_once(__DIR__ . "/../classes/test_$classname.php");
}

$a = new myclass;
var_dump($a);

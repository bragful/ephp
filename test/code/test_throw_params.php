<?php

function hello_world(&$name) {
    $name = "World!";
    print "hello $name\n";
    throw new Exception;
}

$name = "no name!";
hello_world($name);


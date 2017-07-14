<?php

print "_SERVER => ";
var_dump($_SERVER["argv"]);

function whatever() {
    global $PHP_SELF;
    print "GLOBALS => ";
    var_dump($GLOBALS["_GET"]);
    $PHP_SELF = $_SERVER['PHP_SELF'];
    print "_SERVER[argv] => ";
    var_dump($_SERVER["argv"]);
}

print "PHP_SELF => $PHP_SELF\n";
whatever();
print "PHP_SELF => $PHP_SELF\n";

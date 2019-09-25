<?php

function __autoload($classname) {
    print "including... $classname\n";
    require_once(__DIR__ . "/test_$classname.php");
}

function my_autoload($classname) {
    print "not found in __autoload: $classname\n";
}

spl_autoload_register("__autoload");
spl_autoload_register("my_autoload", false, true);
print "BEGIN\n";
spl_autoload_call("myclass");
print "LOADED\n";
#$a = new myclass;
#var_dump($a);

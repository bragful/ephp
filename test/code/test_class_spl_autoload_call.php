<?php

function __autoload($classname) {
    print "including... $classname\n";
    require_once(__DIR__ . "/../classes/test_$classname.php");
}

#spl_autoload_register("__autoload");
print "BEGIN\n";
spl_autoload_call("myclass");
print "LOADED\n";
#$a = new myclass;
#var_dump($a);

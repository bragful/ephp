<?php

$_SERVER = [1,2,3];

class A {
    function print_server() {
        var_dump($_SERVER);
    }
}

function print_server() {
    var_dump($_SERVER);
}

$f = function() {
    var_dump($_SERVER);
};

print "class method:\n";
$a = new A;
$a->print_server();

print "function:\n";
print_server();

print "closure:\n";
$f();

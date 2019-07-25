<?php

namespace My\Full;

class Classname {
    function __construct() {
        print "Namespace => " . __NAMESPACE__ . "\n";
        print "Class => " . __CLASS__ . "\n";
        print "Method => " . __METHOD__ . "\n";
    }
}

namespace My\Full\NSname\subns;

function func() {
    print "running function " . __FUNCTION__ . " from " . __NAMESPACE__ . "\n";
}

namespace Code;

use My\Full\Classname as Another, My\Full\NSname;

print "Running from: " . __NAMESPACE__ . "\n";

$obj = new Another; // instantiates object of class My\Full\Classname
NSname\subns\func(); // calls function My\Full\NSname\subns\func

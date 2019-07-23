<?php

namespace MyCode;

class Hello {
    function __construct() {
        print __METHOD__ . " -- Hello " . WHO . "!\n";
    }
}

function singleton() {
    global $hello;
    if (!$hello) {
        $hello = new Hello();
    } else {
        print "Hello already created!\n";
    }
}

const WHO = 'world';


<?php

namespace Project\Codes;

const OK = 1;
const FAIL = 0;

class Error {
    function __construct() {
        print __NAMESPACE__ . "\n";
    }
}

function errno($n) {
    print __NAMESPACE__ . " | gave => $n\n";
}

namespace Project {
    print "give me an OK => " . Codes\OK . "\n";
    print "give me a FAIL => " . Codes\FAIL . "\n";

    $e = new Codes\Error();
    Codes\errno(Codes\OK);
    Codes\errno(Codes\FAIL);
}

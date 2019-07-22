<?php

namespace MyProject {
    const CONECTAR_OK = 1;
    class Connection { function __construct() { echo "C1: " . __NAMESPACE__ . "\n"; } }
    function connect() { print "F1: " . __NAMESPACE__ . "\n"; }
}

namespace AnotherProject {
    const CONECTAR_OK = 100;
    class Connection { function __construct() { echo "C2: " . __NAMESPACE__ . "\n"; } }
    function connect() { print "F2: " . __NAMESPACE__ . "\n"; }
}

namespace {
    print "\nMyProject namespace:\n";
    print "CONECTAR_OK: " . MyProject\CONECTAR_OK;
    print "\nnew => ";
    $c1 = new MyProject\Connection();
    print "connect => ";
    MyProject\connect();

    print "\nAnotherProject namespace:\n";
    print "CONECTAR_OK: " . AnotherProject\CONECTAR_OK;
    print "\nnew => ";
    $c1 = new AnotherProject\Connection();
    print "connect => ";
    AnotherProject\connect();
}

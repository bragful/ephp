<?php

class C {
    public $a = 200;
    function __construct() {
        print "a => " . $this->a . "\n";
    }
}

class A {
    public $a = 100;
    function __construct() {
        print "a => " . $this->a . "\n";
    }
}

class B extends A {
    function __construct() {
        print "from ";
        parent::__construct();
        print "end\n";

        print "from ";
        A::__construct();
        print "end\n";
    }
}

class D {
    function __construct() {
        C::__construct();
    }
}

$b = new B;
$d = new D;


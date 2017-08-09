<?php

class A {
    public function A() {
        print __CLASS__ . " (A) is constructing!\n";
    }
}

class B {
    public function __construct() {
        print __CLASS__ . " (B) is constructing!\n";
    }
}

class C extends A {}
class D extends B {}
class E extends A {
    public function __construct() {
        parent::__construct();
        print __CLASS__ . " (E) is constructing!\n";
    }
}
class F extends C {}

new C;
new D;
new E;
new F;


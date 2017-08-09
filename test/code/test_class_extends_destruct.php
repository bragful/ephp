<?php

class A {
    public function __destruct() {
        print __CLASS__ . " (A) is being destroyed!\n";
    }
}

class B extends A {}
class C extends A {
    public function __destruct() {
        parent::__destruct();
        print __CLASS__ . " (C) is being destroyed!\n";
    }
}
class D extends B {}

new B;
new C;
new D;
print "END\n";


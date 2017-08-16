<?php

class A {
    public function __invoke() {
        print "Hello world!\n";
    }
}

class B extends A {}
class C {}

$a = new A;
$a();

$b = new B;
$b();

$c = new C;
$c();


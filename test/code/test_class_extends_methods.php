<?php

class A {
    protected function a1() {
        print "calling to protected " . __CLASS__ . " -> a1()\n";
    }

    public function a2() {
        print "calling to public " . __CLASS__ . " -> a2()\n";
    }
}

class B extends A {
    protected function b1() {
        print "calling to protected " . __CLASS__ . " -> b1() who is ";
        $this->a1();
    }

    public function b2() {
        print "calling to public " . __CLASS__ . " -> b2() who is ";
        $this->a2();
    }
}

class C extends B {
    protected function c1() {
        print "calling to protected " . __CLASS__ . " -> c1() who is ";
        $this->b1();
    }

    public function c2() {
        print "calling to public " . __CLASS__ . " -> c2() who is ";
        $this->b2();
    }

    public function calling() {
        $this->c1();
        $this->c2();
    }
}

$c = new C;
$c->calling();


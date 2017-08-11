<?php

class A {
    private function a1() {
        print "calling to protected " . __CLASS__ . " -> a1()\n";
    }

    public function a2() {
        print "calling to public " . __CLASS__ . " -> a2()\n";
    }
}

class B extends A {
    private function b1() {
        print "calling to protected " . __CLASS__ . " -> b1() who is ";
        $this->a1();
    }

    public function b2() {
        print "calling to public " . __CLASS__ . " -> b2() who is ";
        $this->a2();
    }

    public function calling() {
        $this->b1();
        $this->b2();
    }
}

$b = new B;
$b->calling();


<?php

class A {
    final public function name($data) { return "Mike"; }
    public final function phone($data) { return "+1800"; }
}

class B extends A {
    public function name() { return "Roger"; }
    public function phone() { return "+0049"; }
}

$b = new B;
print $b->name() . "\n";


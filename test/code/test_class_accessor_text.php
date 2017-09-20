<?php

class A {
    public function get_name() {
        return "world";
    }

    public function get($what) {
        return $this->{"get_$what"}();
    }
}

$a = new A;
print "hello " . $a->get("name") . "!\n";


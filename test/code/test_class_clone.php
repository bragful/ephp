<?php

class Simple {
    public $a = 20;
    public $name;
    public function __construct() {
        $this->name = "created";
        $this->a += 5;
        print "created!\n";
    }
    public function __clone() {
        $this->name = "cloned";
        $this->a += 10;
        print "cloned!\n";
    }
    public function __destruct() {
        print "destroyed! {$this->name}\n";
    }
}

$s1 = new Simple;
$s2 = clone $s1;

var_dump($s1);
var_dump($s2);

unset($s1);
unset($s2);

print "END\n";


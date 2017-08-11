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
$s3 = new stdClass;
$s3->simple = $s1;
$s4 = clone $s3;

var_dump($s1);
var_dump($s2);
var_dump($s3);
var_dump($s4);

unset($s1);
unset($s2);
unset($s3);
unset($s4);

print "END\n";


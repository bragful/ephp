<?php

class A {
    public $array = [1,2,3,4];
    public $position = 0;

    public function get() {
        return $this->array[$this->position++];
    }
    public function get_error() {
        return $this->array["error"];
    }
}

$a = new A;
while ($data = $a->get()) print $data . "\n";
$a->get_error();


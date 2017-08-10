<?php

class A {
    protected $a = 100;
    public $aa = 100;
}

class B extends A {
    public $b = 200;
    public function change_a($sum) {
        $this->a += $sum;
    }
}

$b = new B;
var_dump($b);

$b->aa += 50;
$b->change_a(25);
var_dump($b);


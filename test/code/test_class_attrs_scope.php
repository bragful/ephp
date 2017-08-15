<?php

class A {
    public $a = 100;
    protected $b = 200;
    private $c = 300;
}

class B extends A {
    public function getting() {
        print "a => $this->a\n";
        print "b => $this->b\n";
        print "c => $this->c\n";
    }
}

$b = new B;
$b->getting();


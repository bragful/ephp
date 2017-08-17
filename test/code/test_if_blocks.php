<?php

if ( ! class_exists('A') ):
class A {
    public $a = 10;

    public function __construct() {
        print "Hello A!\n";
    }
}
endif;

if ( ! class_exists('B') ):
class B extends A {
    public $b = 20;
}
endif;

$b = new B;


<?php

interface A {
    const FOO = 100;
    public function foo();
}

## ONLY IN PHP 7
#interface B implements A {
interface B {
    const BAR = 200;
    public function bar($text);
    public function alice();
}

class C implements A, B {
    public function foo() {
        print self::FOO . "\n";
    }

    public function bar($text) {
        print C::BAR . " $text\n";
    }

    public function alice() {} 
}

$c = new C;
$c->foo();
$c->bar("EUR");

class D implements A, B {}


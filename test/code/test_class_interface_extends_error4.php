<?php

interface A {
    public function abc($a);
}

interface B {
    public function abc($b, $c);
}

class C implements A, B {}


<?php

interface A {
    public function abc($a);
}

class C implements A, A {}


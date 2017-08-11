<?php

class A {
    const FOO = 10;
}


class B extends A {
    const BAR = 20;
}

class C extends B {}

$b = new B;
print "FOO (B) = " . $b::FOO . "\nBAR (B) = " . $b::BAR . "\n";

$c = new C;
print "FOO (C) = " . $c::FOO . "\nBAR (C) = " . $c::BAR . "\n";


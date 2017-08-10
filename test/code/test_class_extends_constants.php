<?php

class A {
    const FOO = 10;
}


class B extends A {
    const BAR = 20;
}

$b = new B;
print "FOO = " .  $b::FOO . "\nBAR = " . $b::BAR . "\n";


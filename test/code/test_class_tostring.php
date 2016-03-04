<?php

class A {}
class B {
    public function __toString() {
        return "B<class>";
    }
}

$a = new A();
$b = new B();

print $b . "\n";
print $a . "\n";


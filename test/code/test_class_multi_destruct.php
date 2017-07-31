<?php

class Simple {
    private $name;
    public function __construct($name = "love") {
        $this->name = $name;
        print "You give {$this->name} a bad name\n";
    }
    public function __destruct() {
        print "Dying {$this->name}\n";
    }
}

$s1 = new Simple;
$s2 = new Simple("hate");
unset($s1);
unset($s2);

print "END\n";


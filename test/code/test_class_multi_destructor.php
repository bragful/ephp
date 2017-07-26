<?php

class Simple {
    public $a = 0;
    public function __construct($a) {
        print "Constructing ($a)...\n";
        $this->a = $a;
    }

    public function __destruct() {
        print "Destructing {$this->a}...\n";
    }
}

$s1 = new Simple("s1");
$s2 = new Simple("s2");
$s3 = array( new Simple("s3"), 1, 2, 3 );
unset($s1);
unset($s3);

print "END\n";

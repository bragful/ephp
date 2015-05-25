<?php

class Simple {

    public function __construct() {
        print "Constructing...\n";
    }

    public function __destruct() {
        print "Destructing...\n";
    }
}

$s1 = new Simple();
$s2 = new Simple();
$s3 = array( new Simple(), 1, 2, 3 );
unset($s1);
unset($s3);

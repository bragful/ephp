<?php

class Simple {

    public function hello() {
        print "Hello world!\n";
    }

}

$s1 = new Simple();
$s2 = new Simple();
var_dump($s1);
var_dump($s2);
unset($s1);
var_dump($s1);
var_dump($s2);


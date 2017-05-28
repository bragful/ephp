<?php

class A {
    public function __destruct() {
        print "destruct!\n";
    }
} 

var_dump([1, 2, (object) [ "b" => 1 ]]);
var_dump(new A);
var_dump((object) [ "a" => 1, "b" => 2 ]);
var_dump((object) [ "a" => [ 1, 2, 3 ] ]);
var_dump((object) [ "a" => (object) [ "b" => 1 ] ]);
var_dump([ "a" => new A ]);

<?php

function isset_check() {
    global $object;
    if (isset($object)) {
        print "isset check: NOOO!!!\n";
    } else {
        print "isset check: OK\n";
    }
}

function missing() {
    global $object;
    $object->help = array("a" => 100);
}

isset_check();
missing();
var_dump($object);

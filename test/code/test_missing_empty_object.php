<?php

function empty_check() {
    global $object;
    if (!empty($object->error)) {
        print "isset check: NOOO!!!\n";
    } else {
        print "isset check: OK\n";
    }
}

function create() {
    global $object;
    $object = new stdClass;
}

create();
empty_check();
var_dump($object);

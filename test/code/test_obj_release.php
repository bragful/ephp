<?php

class ObjTest {}

function reserve() {
    global $obj;
    $obj = new ObjTest();
}

function check() {
    global $obj;
    var_dump($obj);
}

reserve();
check();

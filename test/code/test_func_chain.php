<?php

$data = "hello!\n";

function one() {
    two();
}

function two() {
    global $data;
    print "this is: $data\n";
}

one();

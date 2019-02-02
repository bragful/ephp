<?php

error_reporting(E_ALL);

function early_return() {
    print "we are going to out...\n";
    $data = [];
    if (!isset($data[0])) {
        return(false);
    }
    echo "NOOOOOOO!!!\n";
}

function dying() {
    print "we are going to die!...\n";
    $data = [];
    if (!isset($data[0])) {
        die("dying!");
    }
    echo "NOOOOOOO!!!\n";
}

early_return();
echo "OK\n";
dying();
echo "NO!\n";

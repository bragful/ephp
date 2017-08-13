<?php

$data = true;
if ($data == true) {
    print "OK\n";
} elseif ($data != true) {
    print "NO OK\n";
}

switch ($data) {
    case true: print "OK\n";
    case 1: print "and OK\n";
    default: print "and other OK\n";
}


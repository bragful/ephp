<?php

$data = true;

if($data == false)
    print "NO\n";
// this is a comment
elseif($data)
    print "OK (1)\n";

if ($data) {
    print "OK (2)\n";
} /* more comments */ 
elseif (!$data) {
    print "NO OK\n";
}

switch ($data) {
    case true: print "OK\n";
    case 1: print "and OK\n";
    default: print "and other OK\n";
}


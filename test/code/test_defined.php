<?php

define('TEST_X', 10);

if (defined('TEST_X')) {
    print "TEST_X is " . TEST_X . "\n";
    print "TEST_X is " . constant("TEST_X") . "\n";
}

if (!defined('TEXT_X')) {
    print "TEXT_X is not defined\n";
}

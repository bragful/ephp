<?php

function a($a) {
    b($a + 1);
}

function b($b) {
    c($b + 1);
}

function c($c){
    print "====\n";
    debug_print_backtrace();
    print "----\n";
    debug_print_backtrace(DEBUG_BACKTRACE_IGNORE_ARGS);
    print "----\n";
    debug_print_backtrace(NULL, 2);
    print "====\n";
}

a(0);

<?php

function handle_error($errno, $errstr, $errfile, $errline, $errcontext=array()) {
    static $a = 0;
    $errfile = basename($errfile);
    print "$errfile:$errline [$errno] $errstr\n";
    set_error_handler("handle_error2");
    trigger_error("whoops!", E_USER_ERROR);
}

function handle_error2($errno, $errstr, $errfile, $errline, $errcontext=array()) {
    print "SECOND($errno): $errstr\n";
}

set_error_handler("handle_error");

# generate a warning...
sqrt("a");

trigger_error("Noooo!", E_ERROR);

print "NO OK\n";

<?php

function handle_error($errno, $errstr, $errfile, $errline, $errcontext=array()) {
    static $a = 0;
    $errfile = basename($errfile);
    print "$errfile:$errline [$errno] $errstr\n";
    trigger_error("whoops!", E_USER_ERROR);
}

set_error_handler("handle_error");

# generate a warning...
sqrt("a");

print "NO OK\n";

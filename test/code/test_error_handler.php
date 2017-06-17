<?php

function handle_error($errno, $errstr, $errfile, $errline, $errcontext=array()) {
    static $a = 0;
    $errfile = basename($errfile);
    print "$errfile:$errline [$errno] $errstr\n";
    if ($a++ > 0) return false;
    return true;
}

set_error_handler("handle_error");

# generate a warning...
sqrt("a");
print "-----\n";
sqrt("a");

restore_error_handler();

print "-----\n";
sqrt("a");

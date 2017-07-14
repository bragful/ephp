<?php

function handle_error($errno, $errstr, $errfile, $errline, $errcontext=array()) {
    static $a = 0;
    $errfile = basename($errfile);
    print "$errfile:$errline [$errno] $errstr\n";
    if ($a++ > 0) return false;
    return true;
}

function handle_error2($errno, $errstr, $errfile, $errline, $errcontext=array()) {
    print "wow!\n";
    return false;
}

print "older error handler (0) = " . set_error_handler("handle_error") . "\n";
print "older error handler (1) = " . set_error_handler("older_handle_error") . "\n";

# generate a warning...
sqrt("a");


print "older error handler (2) = " . set_error_handler("handle_error2") . "\n";
print "-----\n";
sqrt("a");

restore_error_handler();

print "-----\n";
sqrt("a");

restore_error_handler();

print "-----\n";
sqrt("a");

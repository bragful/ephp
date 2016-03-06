<?php
$msg = 'hello';

$sample = function() {
    var_dump($msg);
};
$sample();

// Inherit $msg
$sample = function() use ($msg) {
    var_dump($msg);
};
$sample();

// The value is passed as a copy, isn't a reference
$msg = 'world';
$sample();

// Reset message
$msg = 'hello';
print "--$msg--\n";

// Inherit by reference
$sample = function() use (&$msg) {
    var_dump($msg);
};
$sample();

// The value is changed in the parent context
$msg = 'world';
$sample();
print "--$msg--\n";

// The closures also accept normal params
$sample = function($arg) use ($msg) {
    var_dump($arg . ' ' . $msg);
};
$sample("hello");

<?php

set_error_handler(function($errno, $errstr) { print "$errno: $errstr\n"; });

function data(Array $a) { var_dump($a); }
data(15);


set_error_handler(function($errno, $errstr) { print "$errno: $errstr\n"; return false; });

data(25);


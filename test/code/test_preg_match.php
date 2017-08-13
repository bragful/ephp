<?php

var_dump(preg_match('/(foo)(bar)(baz)/', 'foobarbaz'));
var_dump(preg_match('/(foo)(bar)(baz)/', 'fobaba'));
$_SERVER['SERVER_SOFTWARE'] = 'Mozilla';
var_dump(preg_match( '/^Microsoft-IIS\//', $_SERVER['SERVER_SOFTWARE'] ));

$date = 'April 15, 2017';
$pattern = '/(\w+) (\d+), (\d+)/i';
$replacement = '$3-${1}-$2';
echo preg_replace($pattern, $replacement, $date) . "\n";


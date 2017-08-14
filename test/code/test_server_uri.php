<?php

$_SERVER['REQUEST_URI'] = '/index.php?a=100&b=200';
$_SERVER['PHP_SELF'] = $PHP_SELF = preg_replace( '/(\?.*)?$/', '', $_SERVER["REQUEST_URI"] );

print $_SERVER['PHP_SELF'] . "\n";


<?php

ob_start();

print "hello world!";

$text = ob_get_contents();
ob_clean();
print strtoupper($text);
$size = ob_get_length();
print " ($size)\n";
ob_end_flush();

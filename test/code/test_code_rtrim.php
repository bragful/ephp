<?php

$text = "\t\tThese are a few words :) ...  ";
$binary = "\x09Example string\x0A";
$hello  = "Hello World";
var_dump($text, $binary, $hello);

print "\n";

$trimmed = rtrim($text);
var_dump($trimmed);

$chopped = chop($text);
var_dump($chopped);

$trimmed = rtrim($text, " \t.");
var_dump($trimmed);

$chopped = chop($text, " \t.");
var_dump($chopped);

$trimmed = rtrim($hello, "Hdle");
var_dump($trimmed);

$chopped = chop($hello, "Hdle");
var_dump($chopped);

// trim the ASCII control characters at the end of $binary
// (from 0 to 31 inclusive)
$clean = rtrim($binary, "\x00..\x1F");
var_dump($clean);

$clean_chopped = chop($binary, "\x00..\x1F");
var_dump($clean_chopped);


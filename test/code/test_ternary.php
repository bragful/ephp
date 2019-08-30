<?php

$a = ($a < 5) ? 0 : 5;
print "$a\n";

# PHP 7
var_dump($a ?? "no value");
var_dump($b ?? "no value");

<?php

$a = 10;
$b = &$a;
$c = &$a;

var_dump([$a, $b, $c]);


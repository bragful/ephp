<?php

$a = 100;

$b = [&$a, &$a, &$a];
var_dump($b);

$a = 10;
var_dump($b);

unset($a);
var_dump($b);

unset($b[0]);
var_dump($b);

unset($b[1]);
var_dump($b);

unset($b[2]);
var_dump($b);

<?php

$a = new stdClass;
$a->{100} = [1,2,3];

$b = 2;
var_dump($a->{100}[$b]);

$c = 100;
var_dump($a->{$c}{$b});
var_dump($a->{$c});

$c = 101;
var_dump($a->{$c}{$b});


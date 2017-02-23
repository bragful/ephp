<?php

class C { public $hi = "world"; }
$c = new C;

$a = [ "a" => 51  // index a 
     , "b" => 100 // index b
     , "c" => "this is a {$c-> hi }"
     , "d" => (12 + 24 + # this is a comment
               48 + 96 + /* another */
               192 + 384)
     ];
var_dump($a);
var_dump(1 xor 0);
var_dump(20 % 3);
var_dump(5 << 1);
var_dump(5 >> 1);

$a = 10;
$a += 10;
var_dump($a);

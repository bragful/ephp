<?php

$a = [ "1", "2", "3" ];
$b = [];
for ($i=0; $i<count($a); $i++) {
    $b[$i] = (integer)$a[$i];
}
var_dump($a);
var_dump($b);


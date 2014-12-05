<?php

$a = "print_r";
$b = array ( 1, 2, 3 );
$a($b);

$c = array ( "print_r" );
$c[0]($b);

print $c[0]($b);


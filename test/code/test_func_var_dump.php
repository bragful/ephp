<?php

$a = "var_dump";
$b = array ( 1, array( array("a", "b"), 4.5, "hola" ), 3 );
print $a($b);

$c = array ( "var_dump" );
print $c[0]($b);

print $c[0]($b);


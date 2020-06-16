<?php
function foo($a, $b)
{
     $numargs = func_num_args();
     $b *= 10;
     echo "Number of arguments: $numargs\n";
     if ($numargs >= 2) {
         echo "Second argument is: " . func_get_arg(1) . "\n";
     }
}

foo(1, 2, 3);

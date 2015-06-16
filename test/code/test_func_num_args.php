<?php
function foo()
{
    $numargs = func_num_args();
    echo "Num args for <" . __FUNCTION__ . ">: $numargs\n";
}

foo(1, 2, 3);


<?php

function fib($n) {
    $num = 1;
    $prev = 1;
    $res = 0;
    for ($i=0; $i<$n; $i++) {
        $res = $num + $prev;
        $prev = $num;
        $num = $res;
    }
    return $res;
}

print fib(90) . "\n";

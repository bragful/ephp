<?php

function what($data) {
    var_dump($data);
    print " => is " . gettype($data) . "\n";
}

$a = 10.2;
$b = "5.5";

what(true);
what(NULL);
what((int)$a);
what((float)$b);
what("hello");
what([]);
what(new stdClass);
what(NAN);
what(INF);

what(nan);
what(inf);

if ("10" == 10) print "'10' == 10 (OK)\n";
if (10 == "10") print "10 == '10' (OK)\n";
if ("10.1" == 10.1) print "'10.1' == 10.1 (OK)\n";
if (10.1 == "10.1") print "10.1 == '10.1' (OK)\n";

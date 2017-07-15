<?php

if (!$data = myfunct("25"))
    print "NOT ";
print "OK\n";

function myfunct($num) {
    return $num + 10;
}

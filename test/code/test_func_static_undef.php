<?php

function data() {
    static $a;
    if (!isset($a)) {
        print "init \$a = 10\n";
        $a = 10;
    } else {
        $a += 10;
        print "update \$a to $a\n";
    }
}

data();
data();
data();


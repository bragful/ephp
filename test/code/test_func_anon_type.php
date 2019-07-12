<?php

$sample = function($enclose, $data = 10) {
    print $enclose . __FUNCTION__ . "$enclose\n";
};

$sample("|");
print gettype($sample) . "\n";

var_dump($sample);

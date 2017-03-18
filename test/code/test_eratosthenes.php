<?php

define('SIEVE', 1000);

$sieve = [];
for ($i=0; $i<SIEVE; $i++) {
    $sieve[$i] = true;
}

$max = round(sqrt(SIEVE));
for ($i=2; $i<SIEVE; $i++) {
    for ($j=2; $j<=$max; $j++) {
        $sieve[$i*$j] = false;
    }
}

$res = "";
for ($i=2; $i<SIEVE; $i++) {
    if ($sieve[$i]) {
        $res .= ", $i";
    }
}

print "primes: 1$res\n";

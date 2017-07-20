<?php

$primes = [];
for ($i=0; $i<100; $i++) {
    $primes[] = false;
}

$primes[1] = true;
$primes[2] = true;

for ($i=3; $i<100; $i++) {
    $is_prime = true;
    for ($j=2; $j<$i; $j++) {
        if ($i % $j == 0) {
            $is_prime = false;
            break;
        }
    }
    $primes[$i] = $is_prime;
}

print_r($primes);


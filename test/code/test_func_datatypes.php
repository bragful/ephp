<?php

function data(array $data, bool $ok, int $num, float $dec, string $text) {
    foreach ($data as $i) {
        print "$text: $i\n";
    }
}

print data(array(1,2,3,4), true, 10, 10.5, "hello world");
print data("5", "1", true, true, true);


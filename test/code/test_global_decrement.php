<?php

$data = 100;

function decrement() {
    global $data;
    $data --;
    -- $data;
}

for ($i=0; $i<10; $i++) {
    echo "$data\n";
    decrement();
}

<?php

function singleton() {
    global $data;
    if (!$data) {
        echo "There is no data!\n";
        $data = 10;
    } else {
        echo "Data is = $data\n";
        $data ++;
    }
}

var_dump(!$data);

singleton();
singleton();
singleton();

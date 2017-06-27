<?php

function data(array $data, boolean $ok) {
    foreach ($data as $i) {
        print $i . "\n";
    }
}

print data(array(1,2,3,4), true);
print data("5", "1");

<?php

$data = [[1, 2, 3], [10, 20, 30]];

for ($i=0; $i<count($data); $i++) {
    for ($j=0; $j<count($data[$i]); $j++) {
        print $data[$i]{$j} . ", ";
    }
    print "\n";
}


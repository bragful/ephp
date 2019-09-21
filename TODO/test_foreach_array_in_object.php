<?php

$obj = (object)[ "a" => 1, "b" => 2, "data" => [ 1, 2, 3, 4, 5 ] ];
var_dump($obj);

echo "counting ";
foreach ($obj->data as $i) {
    echo "$i, ";
}
echo "and that's all, folks!\n";

echo "not found? ";
foreach ($obj->other_data as $i) {
    echo "$i ";
}
echo "worked?\n";

echo "no object at all?!? ";
foreach ($noobj->no_data as $i) {
    echo "$i ";
}
echo "obviously this won't work!\n";

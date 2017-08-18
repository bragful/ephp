<?php

$records = array(
    array(
        "id" => "Rogue242",
        "name" => "Luke",
        "type" => "Jedi",
    ),
    array(
        "id" => "Halcon321",
        "name" => "Han",
        "type" => "Pusher",
    ),
    array(
        "id" => "Robot2D2",
        "name" => "R2-D2",
        "type" => "Robot",
    )
);

print_r(array_column($records, "name"));
print_r(array_column($records, "name", "id"));
print_r(array_column($records, "surname"));
print_r(array_column($records, "name", "ID"));
print_r(array_column($records, "surname", "ID"));
var_dump(array_column("hello!", "name"));
var_dump(array_column($records, array()));
var_dump(array_column($records, "name", array()));


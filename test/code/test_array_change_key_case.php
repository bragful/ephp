<?php
$input_array = array("FirSt" => 1, "SecOnd" => 4);
print_r(array_change_key_case($input_array, CASE_UPPER));

$input_array = array("one", "two");
print_r(array_change_key_case($input_array, CASE_LOWER));

$input_array = array("fIrSt" => 10, "sEcOnD" => 20);
print_r(array_change_key_case($input_array));
print_r(array_change_key_case($input_array, 100));
var_dump(array_change_key_case("hello!"));

